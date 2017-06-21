"""Transformer of Fortran AST into Python AST."""

import logging
import typing as t
import xml.etree.ElementTree as ET

import typed_ast.ast3 as typed_ast3

_LOG = logging.getLogger(__name__)


class AstTransformer:

    """Transform Fortran AST in XML format into typed Python AST.

    The Fortran AST in XML format is provided by XML output generator for Open Fortran Parser.

    Typed Python AST is provided by typed-ast package.
    """

    def __init__(self):
        self.transforms = [f for f in dir(self) if not f.startswith('__')]

    def transform_all_subnodes(self, node: ET.Element, warn=True):
        transformed = []
        for subnode in node:
            if f'_{subnode.tag}' not in self.transforms:
                if warn:
                    _LOG.warning('%s', ET.tostring(subnode).decode().rstrip())
                continue
            _transform = getattr(self, f'_{subnode.tag}')
            transformed.append(_transform(subnode))
        return transformed

    def _file(self, node: ET.Element) -> typed_ast3.AST:
        body = self.transform_all_subnodes(node)
        return typed_ast3.Module(body=body, type_ignores=[])

    def _module(self, node):
        raise NotImplementedError()

    def _function(self, node: ET.Element):
        raise NotImplementedError()
        #return typed_ast3.FunctionDef()

    def _program(self, node: ET.Element) -> typed_ast3.AST:
        module = typed_ast3.parse('''if __name__ == '__main__':\n    pass''')
        body = self.transform_all_subnodes(node.find('./body'))
        conditional = module.body[0]
        conditional.body = body
        return conditional

    def _loop(self, node: ET.Element):
        if node.attrib['type'] == 'do':
            return self._loop_do(node)
        elif node.attrib['type'] == 'forall':
            return self._loop_forall(node)
        else:
            raise NotImplementedError()

    def _loop_do(self, node: ET.Element):
        target = None
        iter_ = typed_ast3.parse('range(0, 100, 1)', mode='eval')

        #_LOG.warning('loop header:')
        for subnode in node.find('./header'):
            if subnode.tag == 'do-variable':
                target = typed_ast3.Name(id=subnode.attrib['id'], ctx=typed_ast3.Load())
                #break
                continue
            #if f'_{subnode.tag}' not in self.transforms:
            #_LOG.warning('%s', ET.tostring(subnode).decode().rstrip())
            continue
        assert target is not None, ET.tostring(node.find('./header')).decode().rstrip()

        body = self.transform_all_subnodes(node.find('./body'))

        return typed_ast3.For(target=target, iter=iter_, body=body, orelse=[])

    def _loop_forall(self, node: ET.Element):
        target = None
        iter_ = None
        #for subnode in node.find('./header'):
        target = typed_ast3.Name(id="forall_indices", ctx=typed_ast3.Load())
        iter_ = typed_ast3.parse('range(0, 100, 1)', mode='eval')
        body = self.transform_all_subnodes(node.find('./body'))
        return typed_ast3.For(target=target, iter=iter_, body=body, orelse=[])

    def _if(self, node: ET.Element):
        #_LOG.warning('if header:')
        header = self.transform_all_subnodes(node.find('./header'), warn=False)
        if len(header) != 1:
            _LOG.warning('%s', ET.tostring(node).decode().rstrip())
            raise NotImplementedError()
        #if len(header) == 0:
        #    test = typed_ast3.NameConstant(True)
        test = header[0]

        body = self.transform_all_subnodes(node.find('./body'))

        return typed_ast3.If(test=test, body=body, orelse=[])

    def _statement(self, node: ET.Element):
        args = []
        #args.append(typed_ast3.Str(s=ET.tostring(node).decode().rstrip())
        args.append(typed_ast3.Num(n=len(node)))
        return typed_ast3.Expr(value=typed_ast3.Call(
            func=typed_ast3.Name(id='print', ctx=typed_ast3.Load()),
            args=args, keywords=[]))

    def _assignment(self, node: ET.Element):
        _LOG.warning('%s', ET.tostring(node).decode().rstrip())
        raise NotImplementedError()

    def _declaration(self, node: ET.Element):
        _LOG.warning('%s', ET.tostring(node).decode().rstrip())
        raise NotImplementedError()

    def _operation(self, node: ET.Element) -> typed_ast3.AST:
        if node.attrib['type'] == 'binary':
            return self._operation_binary(node)
        _LOG.warning('%s', ET.tostring(node).decode().rstrip())
        raise NotImplementedError()

    def _operation_binary(self, node: ET.Element):
        operand_nodes = node.findall('./operand')
        assert len(operand_nodes) == 2
        operands = []
        for operand in operand_nodes:
            new_operands = self.transform_all_subnodes(operand)
            assert len(new_operands) <= 1 # TODO: change to equality
            operands += new_operands
        assert len(operands) == 2
        operation_type, operator_type = self._operator(node.attrib['operator'])
        if operation_type is typed_ast3.Compare:
            return typed_ast3.Compare(left=operands[0], ops=[operator_type()], comparators=[operands[1]])
        if operation_type is typed_ast3.BinOp:
            return typed_ast3.BinOp(left=operands[0], op=operator_type(), right=operands[1])
        _LOG.warning('%s', ET.tostring(node).decode().rstrip())
        raise NotImplementedError(operation_type)

    def _operator(self, operator: str) -> t.Tuple[t.Type[typed_ast3.AST], t.Type[typed_ast3.AST]]:
        return {
            '+': (typed_ast3.BinOp, typed_ast3.Add),
            '-': (typed_ast3.BinOp, typed_ast3.Sub),
            '*': (typed_ast3.BinOp, typed_ast3.Mult),
            # missing: MatMult
            '/': (typed_ast3.BinOp, typed_ast3.Div),
            '%': (typed_ast3.BinOp, typed_ast3.Mod),
            '**': (typed_ast3.BinOp, typed_ast3.Pow),
            # LShift
            # RShift
            # BitOr
            # BitXor
            # BitAnd
            # missing: FloorDiv
            '==': (typed_ast3.Compare, typed_ast3.Eq),
            '/=': (typed_ast3.Compare, typed_ast3.NotEq),
            '<': (typed_ast3.Compare, typed_ast3.Lt),
            '<=': (typed_ast3.Compare, typed_ast3.LtE),
            '>': (typed_ast3.Compare, typed_ast3.Gt),
            '>=': (typed_ast3.Compare, typed_ast3.GtE),
            # Is
            # IsNot
            # In
            # NotIn
            }[operator]

    def _name(self, node: ET.Element) -> typed_ast3.AST:
        name = typed_ast3.Name(id=node.attrib['id'], ctx=typed_ast3.Load())
        subscripts_node = node.find('./subscripts')
        if not subscripts_node:
            return name
        subscripts = []
        for subscript in subscripts_node.findall('./subscript'):
            new_subscripts = self.transform_all_subnodes(subscript)
            assert len(new_subscripts) <= 1 # TODO: change to equality
            subscripts += new_subscripts
        if len(subscripts) == 1:
            slice_ = typed_ast3.Index(value=subscripts[0])
        else:
            _LOG.warning('%s', ET.tostring(node).decode().rstrip())
            raise NotImplementedError()
        return typed_ast3.Subscript(value=name, slice=slice_, ctx=typed_ast3.Load())

    def _literal(self, node: ET.Element) -> typed_ast3.AST:
        if node.attrib['type'] == 'int':
            return typed_ast3.Num(n=int(node.attrib['value']))
        if node.attrib['type'] == 'char':
            assert len(node.attrib['value']) >= 2
            begin = node.attrib['value'][0]
            end = node.attrib['value'][-1]
            assert begin == end
            return typed_ast3.Str(s=node.attrib['value'][1:-1])
        _LOG.warning('%s', ET.tostring(node).decode().rstrip())
        raise NotImplementedError()


def transform(root_node: ET.Element) -> typed_ast3.AST:
    file_node = root_node[0]
    transformer = AstTransformer()
    return transformer._file(file_node)
