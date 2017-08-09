"""Transformer of Fortran AST into Python AST."""

import collections.abc
import itertools
import logging
import typing as t
import xml.etree.ElementTree as ET

#import numpy as np
import typed_ast.ast3 as typed_ast3
import typed_astunparse

_LOG = logging.getLogger(__name__)


def flatten_sequence(sequence: t.MutableSequence[t.Any]) -> None:
    assert isinstance(sequence, collections.abc.MutableSequence)
    for i, elem in enumerate(sequence):
        if isinstance(elem, collections.abc.Sequence):
            for value in reversed(elem):
                sequence.insert(i, value)
            del sequence[i + len(elem)]

class AstTransformer:

    """Transform Fortran AST in XML format into typed Python AST.

    The Fortran AST in XML format is provided by XML output generator for Open Fortran Parser.

    Typed Python AST is provided by typed-ast package.
    """

    def __init__(self, split_declarations: bool = True):
        self._split_declarations = split_declarations

        self._now_parsing_file = False
        self._top_level_imports = dict()
        self._transforms = [f for f in dir(self) if not f.startswith('__')]

    def _ensure_top_level_import(self, canonical_name: str, alias: t.Optional[str]=None):
        if (canonical_name, alias) not in self._top_level_imports:
            if canonical_name in ('mpif.h', '?'): # TODO: other ways to include MPI?
                self._ensure_mpi_import(canonical_name, alias)
            else:
                self._top_level_imports[canonical_name, alias] = [typed_ast3.Import(
                    names=[typed_ast3.alias(name=canonical_name, asname=alias)])]

    def _ensure_mpi_import(self, canonical_name, alias):
        #if ('mpi4py', None) not in self._top_level_imports:
        self._top_level_imports[canonical_name, alias] = [
            typed_ast3.ImportFrom(
                module='mpi4py', names=[typed_ast3.alias(name='MPI', asname=None)], level=0),
            #typed_ast3.parse('mpi4py.config = no_auto_init', mode='eval') # TODO: may be needed
            ]

    def transform(self, node: ET.Element, warn: bool = True):
        transform_name = '_{}'.format(node.tag.replace('-', '_'))
        if transform_name not in self._transforms:
            if warn:
                _LOG.warning('no transformer available for node "%s"', node.tag)
                _LOG.debug('%s', ET.tostring(node).decode().rstrip())
            return None
        _transform = getattr(self, transform_name)
        return _transform(node)

    def transform_all_subnodes(
            self, node: ET.Element, warn: bool = True, skip_empty: bool = False,
            ignored: t.Set[str] = None):
        transformed = []
        for subnode in node:
            if skip_empty and not subnode.attrib and len(subnode) == 0:
                continue
            transform_name = '_{}'.format(subnode.tag.replace('-', '_'))
            if transform_name not in self._transforms:
                if ignored and subnode.tag in ignored:
                    continue
                if warn:
                    _LOG.warning(
                        'no transformer available for node "%s", a subnode of "%s"',
                        subnode.tag, node.tag)
                    _LOG.debug('%s', ET.tostring(subnode).decode().rstrip())
                    continue
                raise NotImplementedError(
                    'no transformer available for node "{}", a subnode of "{}":\n{}'.format(
                        subnode.tag, node.tag, ET.tostring(node).decode().rstrip()))
            _transform = getattr(self, transform_name)
            transformed.append(_transform(subnode))
        return transformed

    def _file(self, node: ET.Element) -> t.Union[typed_ast3.Module, typed_ast3.Expr]:
        if not self._now_parsing_file:
            self._now_parsing_file = True
            body = self.transform_all_subnodes(
                node, warn=False, ignored={'start-of-file', 'end-of-file'})
            self._now_parsing_file = False
            import_statements = list(itertools.chain(
                *[statements for _, statements in self._top_level_imports.items()]))
            body = import_statements + body
        else:
            return typed_ast3.Expr(value=typed_ast3.Call(
                func=typed_ast3.Name(id='print'),
                args=[typed_ast3.Str(s='file'), typed_ast3.Str(s=node.attrib['path'])],
                keywords=[]))
        return typed_ast3.Module(body=body, type_ignores=[])

    def _module(self, node: ET.Element):
        module = typed_ast3.parse('''if __name__ == '__main__':\n    pass''')
        body = self.transform_all_subnodes(node.find('./body'))
        conditional = module.body[0]
        conditional.body = body
        members_node = node.find('./members')
        if members_node is None:
            return conditional
        members = self.transform_all_subnodes(members_node)
        #if not members:
        _LOG.warning('%s', ET.tostring(node).decode().rstrip())
        raise NotImplementedError()
        conditional.body = body + members
        return module

    def _function(self, node: ET.Element):
        arguments = self.transform(node.find('./header/names'))
        body = self.transform_all_subnodes(node.find('./body'))
        return typed_ast3.FunctionDef(
            name=node.attrib['name'], args=arguments, body=body, decorator_list=[],
            returns=typed_ast3.NameConstant(None))

    def _subroutine(self, node: ET.Element) -> typed_ast3.FunctionDef:
        header_node = node.find('./header')
        if header_node is None:
            raise SyntaxError(
                'no "header" found in "subroutine":\n{}'
                .format(ET.tostring(node).decode().rstrip()))
        arguments_node = node.find('./header/arguments')
        if arguments_node is None:
            arguments = []
        else:
            arguments = self.transform(arguments_node)
        body = self.transform_all_subnodes(node.find('./body'))
        return typed_ast3.FunctionDef(
            name=node.attrib['name'], args=arguments, body=body, decorator_list=[],
            returns=typed_ast3.NameConstant(None))

    def _arguments(self, node: ET.Element) -> typed_ast3.arguments:
        return typed_ast3.arguments(
            args=self.transform_all_subnodes(
                node, warn=False, ignored={
                    'dummy-arg-list__begin', 'dummy-arg-list'} | {'generic-name-list__begin', 'generic-name-list'}),
            vararg=None, kwonlyargs=[], kwarg=None, defaults=[], kw_defaults=[])

    def _argument(self, node: ET.Element) -> typed_ast3.arg:
        if 'name' not in node.attrib:
            raise SyntaxError(
                '"name" attribute not present in:\n{}'.format(ET.tostring(node).decode().rstrip()))
        self._ensure_top_level_import('typing', 't')
        return typed_ast3.arg(
            arg=node.attrib['name'], annotation=typed_ast3.Attribute(
                value=typed_ast3.Name(id='t', ctx=typed_ast3.Load()),
                attr='Any', ctx=typed_ast3.Load()))

    def _program(self, node: ET.Element) -> typed_ast3.AST:
        module = typed_ast3.parse('''if __name__ == '__main__':\n    pass''')
        body = self.transform_all_subnodes(node.find('./body'))
        for i in range(len(body) - 1, -1, -1):
            if isinstance(body[i], list):
                sublist = body[i]
                del body[i]
                for elem in reversed(sublist):
                    body.insert(i, elem)
        conditional = module.body[0]
        conditional.body = body
        return conditional

    def _specification(self, node: ET.Element) -> t.List[typed_ast3.AST]:
        declarations = self.transform_all_subnodes(
            node, warn=False, skip_empty=True,
            ignored={'declaration-construct', 'specification-part'})
        return declarations

    def _declaration(self, node: ET.Element) -> typed_ast3.AnnAssign:
        if 'type' not in node.attrib:
            return [] # TODO: TMP
            raise SyntaxError(
                '"type" attribute not present in:\n{}'.format(ET.tostring(node).decode().rstrip()))
        if node.attrib['type'] == 'implicit':
            # TODO: generate comment here maybe?
            if node.attrib['subtype'].lower() == 'none':
                annotation = typed_ast3.NameConstant(value=None)
            else:
                annotation = typed_ast3.Str(s=node.attrib['subtype'])
            return typed_ast3.AnnAssign(
                target=typed_ast3.Name(id='implicit'), annotation=annotation, value=None,
                simple=True)
        elif node.attrib['type'] == 'variable':
            return self._declaration_variable(node)
        elif node.attrib['type'] == 'include':
            return self._declaration_include(node)
        return typed_ast3.Expr(value=typed_ast3.Call(
            func=typed_ast3.Name(id='print'),
            args=[typed_ast3.Str(s='declaration'), typed_ast3.Str(s=node.attrib['type'])],
            keywords=[]))

    def _declaration_variable(
            self, node: ET.Element) -> t.Union[
                typed_ast3.Assign, typed_ast3.AnnAssign, t.List[typed_ast3.Assign],
                t.List[typed_ast3.AnnAssign]]:
        variables_node = node.find('./variables')
        if variables_node is None:
            _LOG.error('%s', ET.tostring(node).decode().rstrip())
            raise SyntaxError('"variables" node not present')
        variables = self.transform_all_subnodes(
            variables_node, warn=False, skip_empty=True,
            ignored={'entity-decl-list__begin', 'entity-decl-list'})
        if not variables:
            _LOG.error('%s', ET.tostring(node).decode().rstrip())
            raise SyntaxError('at least one variable expected in variables list')
        if len(variables) == 1:
            target, value = variables[0]
        else:
            target = typed_ast3.Tuple(elts=[var for var, _ in variables])
            value = [val for _, val in variables]

        type_node = node.find('./type')
        if type_node is None:
            _LOG.error('%s', ET.tostring(node).decode().rstrip())
            raise SyntaxError('"type" node not present')
        annotation = self.transform(type_node)

        dimensions_node = node.find('./dimensions')
        dimensions = None
        if dimensions_node is not None:
            dimensions = self.transform_all_subnodes(dimensions_node, ignored={'array-spec'})
            assert len(dimensions) >= 1
            data_type = annotation
            self._ensure_top_level_import('static_typing', 'st')
            annotation = typed_ast3.Subscript(
                value=typed_ast3.Attribute(
                    value=typed_ast3.Name(id='st', ctx=typed_ast3.Load()),
                    attr='ndarray', ctx=typed_ast3.Load()),
                slice=typed_ast3.ExtSlice(dims=[typed_ast3.Num(n=len(dimensions)), data_type]),
                ctx=typed_ast3.Load())
            if isinstance(value, list) and not all([_ is None for _ in value]):
                _LOG.warning('%s', value)
                _LOG.warning('%s', ET.tostring(node).decode().rstrip())
                raise NotImplementedError()
            elif not isinstance(value, list) and value is not None:
                _LOG.warning('%s', ET.tostring(node).decode().rstrip())
                raise NotImplementedError()
            else:
                self._ensure_top_level_import('numpy', 'np')
                for i, (var, val) in enumerate(variables):
                    val = typed_ast3.Call(
                        func=typed_ast3.Attribute(
                            value=typed_ast3.Name(id='np'), attr='ndarray', ctx=typed_ast3.Load()),
                        args=[typed_ast3.Tuple(elts=dimensions)],
                        keywords=[typed_ast3.keyword(arg='dtype', value=data_type)])
                    variables[i] = (var, val)
                #value = [ for _ in variables]
                value = [val for _, val in variables]

        if len(variables) == 1:
            return typed_ast3.AnnAssign(
                target=target, annotation=annotation, value=value, simple=True)
        else:
            assert len(variables) == len(value)
            if not self._split_declarations:
                value = typed_ast3.Tuple(
                    elts=[typed_ast3.NameConstant(value=None) if v is None else v for v in value])
                type_comment = typed_astunparse.unparse(
                    typed_ast3.Tuple(elts=[annotation for _ in range(len(variables))])).strip()
                return typed_ast3.Assign(targets=[target], value=value, type_comment=type_comment)
            return [
                typed_ast3.AnnAssign(target=var, annotation=annotation, value=val, simple=True)
                for var, val in variables]

    def _declaration_include(self, node: ET.Element):
        file_node = node.find('./file')
        path_attrib = file_node.attrib['path']
        self._ensure_top_level_import(path_attrib)
        return typed_ast3.Import(names=[typed_ast3.alias(name=path_attrib, asname=None)])

    def _loop(self, node: ET.Element):
        if node.attrib['type'] == 'do':
            return self._loop_do(node)
        #elif node.attrib['type'] == 'do-while':
        #    return self._loop_do_while(node)
        elif node.attrib['type'] == 'forall':
            return self._loop_forall(node)
        else:
            _LOG.error('%s', ET.tostring(node).decode().rstrip())
            raise NotImplementedError()

    def _loop_do(self, node: ET.Element) -> typed_ast3.For:
        index_variable = node.find('./header/index-variable')
        target, iter_ = self._index_variable(index_variable)
        body = self.transform_all_subnodes(node.find('./body'), ignored={'block'})
        return typed_ast3.For(target=target, iter=iter_, body=body, orelse=[])

    def _loop_do_while(self, node: ET.Element) -> typed_ast3.While:
        pass

    def _loop_forall(self, node: ET.Element) -> typed_ast3.For:
        index_variables = node.find('./header/index-variables')
        outer_loop = None
        inner_loop = None
        for index_variable in index_variables.findall('./index-variable'):
            if not index_variable:
                continue # TODO: this is just a duct tape
            target, iter_ = self._index_variable(index_variable)
            if outer_loop is None:
                outer_loop = typed_ast3.For(target=target, iter=iter_, body=[], orelse=[])
                inner_loop = outer_loop
                continue
            inner_loop.body = [typed_ast3.For(target=target, iter=iter_, body=[], orelse=[])]
            inner_loop = inner_loop.body[0]
        inner_loop.body = self.transform_all_subnodes(node.find('./body'))
        return outer_loop

    def _index_variable(self, node: ET.Element) -> t.Tuple[typed_ast3.Name, typed_ast3.Call]:
        target = typed_ast3.Name(id=node.attrib['name'], ctx=typed_ast3.Load())
        lower_bound = node.find('./lower-bound')
        upper_bound = node.find('./upper-bound')
        step = node.find('./step')
        range_args = []
        if lower_bound is not None:
            args = self.transform_all_subnodes(lower_bound)
            assert len(args) == 1, args
            range_args.append(args[0])
        if upper_bound is not None:
            args = self.transform_all_subnodes(upper_bound)
            assert len(args) == 1, args
            range_args.append(typed_ast3.BinOp(
                left=args[0], op=typed_ast3.Add(), right=typed_ast3.Num(n=1)))
        if step is not None:
            args = self.transform_all_subnodes(step)
            assert len(args) == 1, args
            range_args.append(args[0])
        iter_ = typed_ast3.Call(
            func=typed_ast3.Name(id='range', ctx=typed_ast3.Load()),
            args=range_args, keywords=[])
        return target, iter_

    def _if(self, node: ET.Element):
        #_LOG.warning('if header:')
        header = self.transform_all_subnodes(
            node.find('./header'), warn=False,
            ignored={'executable-construct', 'execution-part-construct'})
        if len(header) != 1:
            _LOG.warning('%s', ET.tostring(node).decode().rstrip())
            _LOG.error([typed_astunparse.unparse(_).rstrip() for _ in header])
            raise NotImplementedError()
        #if len(header) == 0:
        #    test = typed_ast3.NameConstant(True)
        test = header[0]

        body = self.transform_all_subnodes(node.find('./body'), skip_empty=True, ignored={'block'})

        return typed_ast3.If(test=test, body=body, orelse=[])

    def _statement(self, node: ET.Element):
        details = self.transform_all_subnodes(
            node, ignored={
                'action-stmt', 'executable-construct', 'execution-part-construct',
                'execution-part'})
        flatten_sequence(details)
        if len(details) == 0:
            args = [
                typed_ast3.Str(s=ET.tostring(node).decode().rstrip()),
                typed_ast3.Num(n=len(node))]
            return [
                typed_ast3.Expr(value=typed_ast3.Call(
                    func=typed_ast3.Name(id='print', ctx=typed_ast3.Load()),
                    args=args, keywords=[])),
                typed_ast3.Pass()]
        return [
            detail if isinstance(detail, (typed_ast3.Expr, typed_ast3.Assign, typed_ast3.AnnAssign))
            else typed_ast3.Expr(value=detail)
            for detail in details]

    def _call(self, node: ET.Element) -> t.Union[typed_ast3.Call, typed_ast3.Assign]:
        called = self.transform_all_subnodes(node, warn=False, ignored={'call-stmt'})
        if len(called) != 1:
            _LOG.warning('%s', ET.tostring(node).decode().rstrip())
            _LOG.error('%s', [typed_astunparse.unparse(_).rstrip() for _ in called])
            raise SyntaxError("call statement must contain a single called object")
        if isinstance(called[0], typed_ast3.Call):
            call = called[0]
        else:
            _LOG.warning('called an ambiguous node')
            _LOG.warning('%s', ET.tostring(node).decode().rstrip())
            func = called[0]
            #assert name.tag == 'name' or name.
            args = []
            #if isinstance(name, typed_ast3.Subscript):
            #args = node.findall('./name/subscripts/subscript')
            call = typed_ast3.Call(func=func, args=args, keywords=[])
        if isinstance(call.func, typed_ast3.Name) and call.func.id.startswith('MPI_'):
            call = self._transform_mpi_call(call)
        return call

    def _write(self, node) -> t.Union[typed_ast3.Expr, typed_ast3.Assign]:
        args = []
        written = []
        io_controls_node = node.find('./io-controls')
        if io_controls_node is not None:
            args = self.transform_all_subnodes(
                io_controls_node, skip_empty=True,
                ignored={'io-control-spec-list__begin', 'io-control-spec-list'})
        outputs_node = node.find('./outputs')
        if outputs_node is not None:
            written = self.transform_all_subnodes(
                outputs_node, skip_empty=True,
                ignored={'output-item-list__begin', 'output-item', 'output-item-list'})
        if len(written) > 1 or len(args) > 1:
            # file
            pass
        else:
            # string
            pass
        args += written
        return typed_ast3.Expr(value=typed_ast3.Call(
            func=typed_ast3.Name(id='write', ctx=typed_ast3.Load()),
            args=args, keywords=[]))

    def _io_control(self, node) -> typed_ast3.AST:
        io_control = self.transform_all_subnodes(node)
        if len(node) == 0 and not node.attrib['argument-name']:
            return [] # TODO: TMP
        if len(io_control) != 1:
            _LOG.warning('%s', ET.tostring(node).decode().rstrip())
            raise NotImplementedError()
        if node.attrib['argument-name']:
            return typed_ast3.keyword(arg=node.attrib['argument-name'], value=io_control[0])
        return io_control[0]

    def _print(self, node):
        outputs_node = node.find('./outputs')
        args = []
        if outputs_node is not None:
            args = self.transform_all_subnodes(
                outputs_node, skip_empty=True,
                ignored={'output-item-list__begin', 'output-item', 'output-item-list'})
        return typed_ast3.Expr(value=typed_ast3.Call(
            func=typed_ast3.Name(id='print', ctx=typed_ast3.Load()),
            args=args, keywords=[]))

    def _output(self, node):
        output = self.transform_all_subnodes(node)
        if len(output) != 1:
            _LOG.warning('%s', ET.tostring(node).decode().rstrip())
            raise NotImplementedError()
        return output[0]

    def _transform_mpi_call(
            self, tree: typed_ast3.Call) -> t.Union[typed_ast3.Call, typed_ast3.Assign]:
        assert isinstance(tree, typed_ast3.Call)
        assert tree.func.id.startswith('MPI_')
        assert len(tree.func.id) > 4
        core_name = typed_ast3.Name(id='MPI')
        mpi_function_name = tree.func.id[4] + tree.func.id[5:].lower()
        assert len(tree.args) > 0
        # extract last arg -- it's error var
        error_var = tree.args.pop(-1)
        assert isinstance(error_var, typed_ast3.Name), (type(error_var), error_var)
        if mpi_function_name in ['Comm_size', 'Comm_rank', 'Barrier']:
            # extract 1st arg - in some cases it's the MPI scope
            mpi_comm = tree.args.pop(0)
            assert isinstance(mpi_comm, typed_ast3.Name)
            assert mpi_comm.id.startswith('MPI_')
            assert len(mpi_comm.id) > 4
            core_name = typed_ast3.Attribute(
                value=core_name, attr=mpi_comm.id[4:], ctx=typed_ast3.Load())
        tree.func = typed_ast3.Attribute(
            value=core_name, attr=mpi_function_name, ctx=typed_ast3.Load())
        # create assignment of call result to its current 1st var
        if tree.args:
            var = tree.args.pop(0)
            tree = typed_ast3.Assign(targets=[var], value=tree, type_comment=None)
        return [tree, typed_ast3.AnnAssign(
            target=error_var, value=None, annotation=typed_ast3.Str(s='MPI error code'), simple=1)]
        #_LOG.error('%s', typed_astunparse.unparse(tree).rstrip())
        #raise NotImplementedError()

    def _assignment(self, node: ET.Element):
        target = self.transform_all_subnodes(node.find('./target'))
        value = self.transform_all_subnodes(node.find('./value'))
        if len(target) != 1: 
            raise SyntaxError(
                'exactly 1 target expected but {} given {} in:\n{}'
                .format(len(target), target, ET.tostring(node).decode().rstrip()))
        if len(value) != 1:
            raise SyntaxError(
                'exactly 1 value expected but {} given {} in:\n{}'
                .format(len(value), value, ET.tostring(node).decode().rstrip()))
        return typed_ast3.Assign(targets=[target], value=value, type_comment=None)

    def _operation(self, node: ET.Element) -> typed_ast3.AST:
        if node.attrib['type'] == 'multiary':
            return self._operation_multiary(node)
        if node.attrib['type'] == 'unary':
            return self._operation_unary(node)
        _LOG.warning('%s', ET.tostring(node).decode().rstrip())
        raise NotImplementedError()

    def _operation_multiary(
            self, node: ET.Element) -> t.Union[
                typed_ast3.BinOp, typed_ast3.BoolOp, typed_ast3.Compare]:
        operators_and_operands = self.transform_all_subnodes(
            node, skip_empty=True, ignored={
                'add-operand__add-op', 'add-operand', 'mult-operand__mult-op', 'mult-operand',
                'primary'})
        assert isinstance(operators_and_operands, list), operators_and_operands
        assert len(operators_and_operands) % 2 == 1, operators_and_operands

        operation_type, _ = operators_and_operands[1]
        if operation_type is typed_ast3.BinOp:
            return self._operation_multiary_arithmetic(operators_and_operands)
        if operation_type is typed_ast3.BoolOp:
            return self._operation_multiary_boolean(operators_and_operands)
        if operation_type is typed_ast3.Compare:
            return self._operation_multiary_comparison(operators_and_operands)
        raise NotImplementedError()

    def _operation_multiary_arithmetic(
            self, operators_and_operands: t.Sequence[t.Union[typed_ast3.AST, t.Tuple[
                t.Type[typed_ast3.BinOp], t.Type[typed_ast3.AST]]]]) -> typed_ast3.BinOp:
        operators_and_operands = list(reversed(operators_and_operands))
        operators_and_operands += [(None, None)]

        root_operation = None
        root_operation_type = None
        root_operator_type = None
        zippped = zip(operators_and_operands[::2], operators_and_operands[1::2])
        for operand, (operation_type, operator_type) in zippped:
            if root_operation is None:
                root_operation_type = operation_type
                root_operator_type = operator_type
                if root_operation_type is not typed_ast3.BinOp:
                    raise NotImplementedError('root operation initialisation')
                root_operation = typed_ast3.BinOp(
                    left=None, op=root_operator_type(), right=operand)
                operation = root_operation
                continue
            if operation_type is not None:
                assert operation_type is root_operation_type, (operation_type, root_operation_type)
                operation.left = typed_ast3.BinOp(left=None, op=operator_type(), right=operand)
                operation = operation.left
            else:
                operation.left = operand

        return root_operation

    def _operation_multiary_boolean(
            self, operators_and_operands: t.Sequence[t.Union[typed_ast3.AST, t.Tuple[
                t.Type[typed_ast3.BoolOp], t.Type[typed_ast3.AST]]]]) -> typed_ast3.BoolOp:
        operators_and_operands += [(None, None)]

        root_operation = None
        root_operation_type = None
        root_operator_type = None
        zippped = zip(operators_and_operands[::2], operators_and_operands[1::2])
        for operand, (operation_type, operator_type) in zippped:
            if root_operation is None:
                root_operation_type = operation_type
                root_operator_type = operator_type
                root_operation = typed_ast3.BoolOp(
                    op=root_operator_type(), values=[operand])
                continue
            if operation_type is not None:
                assert operation_type is root_operation_type, (operation_type, root_operation_type)
                assert operator_type is root_operator_type, (operator_type, root_operator_type)
                root_operation.values.append(operand)

        return root_operation

    def _operation_multiary_comparison(
            self, operators_and_operands: t.Sequence[t.Union[typed_ast3.AST, t.Tuple[
                t.Type[typed_ast3.Compare], t.Type[typed_ast3.AST]]]]) -> typed_ast3.Compare:
        assert len(operators_and_operands) == 3, operators_and_operands
        left_operand, (operation_type, operator_type), right_operand = operators_and_operands
        assert operation_type is typed_ast3.Compare
        return typed_ast3.Compare(
            left=left_operand, ops=[operator_type()], comparators=[right_operand])

    def _operation_unary(self, node: ET.Element):
        operators_and_operands = self.transform_all_subnodes(
            node, skip_empty=True) #, ignored={
            #    'add-operand__add-op', 'add-operand', 'mult-operand__mult-op', 'mult-operand',
            #    'primary'})
        assert isinstance(operators_and_operands, list), operators_and_operands
        assert len(operators_and_operands) == 2, operators_and_operands
        operation_type, operator_type = operators_and_operands[0]
        if operation_type is typed_ast3.BinOp:
            operation_type, operator_type = {
                (typed_ast3.BinOp, typed_ast3.Add): (typed_ast3.UnaryOp, typed_ast3.UAdd),
                (typed_ast3.BinOp, typed_ast3.Sub): (typed_ast3.UnaryOp, typed_ast3.USub)
                }[operation_type, operator_type]
        operand = operators_and_operands[1]
        return operation_type(op=operator_type(), operand=operand)

    def _operand(self, node: ET.Element) -> t.Any:
        operand = self.transform_all_subnodes(node) #, ignored={'power-operand'})
        if len(operand) != 1:
            _LOG.warning('%s', ET.tostring(node).decode().rstrip())
            #_LOG.error("%s", operand)
            _LOG.error([typed_astunparse.unparse(_).rstrip() for _ in operand])
            raise SyntaxError(
                'expected exactly one operand but got {} in:\n{}'
                .format(len(operand), ET.tostring(node).decode().rstrip()))
        return operand[0]

    def _operator(
            self, node: ET.Element) -> t.Tuple[t.Type[typed_ast3.AST], t.Type[typed_ast3.AST]]:
        return {
            # binary
            '+': (typed_ast3.BinOp, typed_ast3.Add),
            '-': (typed_ast3.BinOp, typed_ast3.Sub),
            '*': (typed_ast3.BinOp, typed_ast3.Mult),
            # missing: MatMult
            '/': (typed_ast3.BinOp, typed_ast3.Div),
            '%': (typed_ast3.BinOp, typed_ast3.Mod),
            '**': (typed_ast3.BinOp, typed_ast3.Pow),
            '//': (typed_ast3.BinOp, typed_ast3.Add), # concatenation operator, only in Fortran
            # LShift
            # RShift
            # BitOr
            # BitXor
            # BitAnd
            # missing: FloorDiv
            '.eq.': (typed_ast3.Compare, typed_ast3.Eq),
            '==': (typed_ast3.Compare, typed_ast3.Eq),
            '.ne.': (typed_ast3.Compare, typed_ast3.NotEq),
            '/=': (typed_ast3.Compare, typed_ast3.NotEq),
            '.lt.': (typed_ast3.Compare, typed_ast3.Lt),
            '<': (typed_ast3.Compare, typed_ast3.Lt),
            '.le.': (typed_ast3.Compare, typed_ast3.LtE),
            '<=': (typed_ast3.Compare, typed_ast3.LtE),
            '.gt.': (typed_ast3.Compare, typed_ast3.Gt),
            '>': (typed_ast3.Compare, typed_ast3.Gt),
            '.ge.': (typed_ast3.Compare, typed_ast3.GtE),
            '>=': (typed_ast3.Compare, typed_ast3.GtE),
            # Is
            # IsNot
            # In
            # NotIn
            '.and.': (typed_ast3.BoolOp, typed_ast3.And),
            '.or.': (typed_ast3.BoolOp, typed_ast3.Or),
            # unary
            # '+': (typed_ast3.UnaryOp, typed_ast3.UAdd),
            # '-': (typed_ast3.UnaryOp, typed_ast3.USub),
            '.not.': (typed_ast3.UnaryOp, typed_ast3.Not),
            # Invert: (typed_ast3.UnaryOp, typed_ast3.Invert)
            }[node.attrib['operator'].lower()]

    def _array_constructor(self, node: ET.Element) -> typed_ast3.ListComp:
        values = node.findall('./value')
        if len(values) != 2:
            raise NotImplementedError('not implemented handling of:\n{}'.format(ET.tostring(node).decode().rstrip()))
        value = values[0]
        sub_values = value.find('./array-constructor-values')
        header_node = value.find('./header')
        header = self.transform_all_subnodes(header_node, warn=False)
        assert len(header) == 1
        comp_target, comp_iter = header[0]
        return typed_ast3.ListComp(
            elt=typed_ast3.Call(
                func=typed_ast3.Name(id='do_nothing', ctx=typed_ast3.Load()),
                args=[], keywords=[], starargs=None, kwargs=None),
            generators=[
                typed_ast3.comprehension(target=comp_target, iter=comp_iter, ifs=[], is_async=0)])

        # "[ord(c) for line in file for c in line]"
        #_(elt=Call(func=Name(id='ord', ctx=Load()), args=[
        #  Name(id='c', ctx=Load()),
        #], keywords=[], starargs=None, kwargs=None), generators=[
        #  comprehension(target=Name(id='line', ctx=Store()), iter=Name(id='file', ctx=Load()), ifs=[], is_async=0),
        #  comprehension(target=Name(id='c', ctx=Store()), iter=Name(id='line', ctx=Load()), ifs=[], is_async=0),
        #])

    def _dimension(self, node: ET.Element) -> t.Union[typed_ast3.Num, typed_ast3.Index]:
        dim_type = node.attrib['type']
        if dim_type == 'simple':
            values = self.transform_all_subnodes(node, ignored={'array-spec-element'})
            if len(values) != 1:
                _LOG.error('simple dimension should have exactly one value, but it has %i', len(values))
            return typed_ast3.Index(value=values[0])
        elif dim_type == 'assumed-shape':
            return typed_ast3.Slice(lower=None, upper=None, step=None)
        else:
            _LOG.warning('%s', ET.tostring(node).decode().rstrip())
            raise NotImplementedError('dimension type "{}" not supported'.format(dim_type))

    _basic_types = {
        ('logical', None): 'bool',
        ('integer', None): 'int',
        ('real', None): 'float',
        ('character', t.Any): 'str',
        ('integer', 4): 'np.int32',
        ('integer', 8): 'np.int64',
        ('real', 4): 'np.float32',
        ('real', 8): 'np.float64'}

    def _type(self, node: ET.Element) -> type:
        name = node.attrib['name'].lower()
        length = \
            self.transform(node.find('./length')) if node.attrib['hasLength'] == "true" else None
        kind = self.transform(node.find('./kind')) if node.attrib['hasKind'] == "true" else None
        if length is not None and kind is not None:
            raise SyntaxError(
                'only  one of "length" and "kind" can be provided, but both were given'
                ' ("{}" and "{}" respectively) in:\n{}'
                .format(length, kind, ET.tostring(node).decode().rstrip()))
        if name == 'character':
            if length is not None:
                if isinstance(length, typed_ast3.Num):
                    length = length.n
                _LOG.warning(
                    'ignoring string length "%i" in:\n%s',
                    length, ET.tostring(node).decode().rstrip())
            return typed_ast3.parse(self._basic_types[name, t.Any], mode='eval')
        elif length is not None:
            self._ensure_top_level_import('numpy', 'np')
            return typed_ast3.parse(self._basic_types[name, length], mode='eval')
        elif kind is not None:
            self._ensure_top_level_import('numpy', 'np')
            if isinstance(kind, typed_ast3.Num):
                kind = kind.n
            if not isinstance(kind, int):
                #_LOG.warning('%s', ET.tostring(node).decode().rstrip())
                #raise NotImplementedError('non-literal kinds are not supported')
                python_type = typed_ast3.parse(self._basic_types[name, None], mode='eval')
                self._ensure_top_level_import('static_typing', 'st')
                static_type = typed_ast3.Attribute(
                    value=typed_ast3.Name(id='st', ctx=typed_ast3.Load()),
                    attr=python_type, ctx=typed_ast3.Load())
                return typed_ast3.Subscript(
                    value=static_type, slice=typed_ast3.Index(value=kind), ctx=typed_ast3.Load())
                #typed_ast3.parse({
                #    'integer': 'st.int[0]'.format(kind),
                #    'real': lambda kind: 'st.float[0]'.format(kind)}[name](kind), mode='eval')
                #custom_kind_type.
                #return custom_kind_type
            return typed_ast3.parse(self._basic_types[name, kind], mode='eval')
        else:
            return typed_ast3.parse(self._basic_types[name, None], mode='eval')
        _LOG.warning('%s', ET.tostring(node).decode().rstrip())
        raise NotImplementedError()

    def _length(self, node):
        values = self.transform_all_subnodes(node, ignored={'char-length'})
        if len(values) == 1:
            return values[0]
        _LOG.warning('%s', ET.tostring(node).decode().rstrip())
        raise NotImplementedError()

    def _kind(self, node):
        values = self.transform_all_subnodes(node, ignored={'kind-selector'})
        if len(values) == 1:
            return values[0]
        if 'value' in node.attrib:
            return int(node.attrib['value'])
        _LOG.warning('%s', ET.tostring(node).decode().rstrip())
        raise NotImplementedError()

    def _variable(self, node: ET.Element) -> t.Tuple[
            typed_ast3.Name, t.Any]:
        value_node = node.find('./initial-value')
        value = None
        if value_node is not None:
            values = self.transform_all_subnodes(value_node, warn=False, ignored={'initialization'})
            assert len(values) == 1, values
            value = values[0]
        return typed_ast3.Name(id=node.attrib['name']), value

    def _names(self, node: ET.Element) -> typed_ast3.arguments:
        return self._arguments(node)

    def _name(self, node: ET.Element) -> typed_ast3.AST:
        name = typed_ast3.Name(id=node.attrib['id'], ctx=typed_ast3.Load())
        if 'type' in node.attrib:
            name_type = node.attrib['type']
        else:
            name_type = None
            #_LOG.warning('%s', ET.tostring(node).decode().rstrip())
            #raise NotImplementedError()
        subscripts_node = node.find('./subscripts')
        if name_type == "procedure":
            args = self._args(subscripts_node) if subscripts_node else []
            return typed_ast3.Call(func=name, args=args, keywords=[])
        if not subscripts_node:
            return name
        slice_ = self._subscripts(subscripts_node)
        if not slice_:
            return typed_ast3.Call(func=name, args=[], keywords=[])
        return typed_ast3.Subscript(value=name, slice=slice_, ctx=typed_ast3.Load())

    def _args(self, node: ET.Element, arg_node_name: str = 'subscript') -> t.List[typed_ast3.AST]:
        args = []
        for arg_node in node.findall(f'./{arg_node_name}'):
            new_args = self.transform_all_subnodes(
                arg_node, warn=False, skip_empty=True,
                ignored={'section-subscript', 'actual-arg', 'actual-arg-spec', 'argument'})
            if not new_args:
                continue
            if len(new_args) != 1:
                _LOG.error('%s', ET.tostring(arg_node).decode().rstrip())
                _LOG.error('%s', [typed_astunparse.unparse(_) for _ in new_args])
                raise SyntaxError('args must be specified one new arg at a time')
            args += new_args
        return args

    def _subscripts(
            self, node: ET.Element) -> t.Union[
                typed_ast3.Index, typed_ast3.Slice, typed_ast3.ExtSlice]:
        subscripts = []
        for subscript in node.findall('./subscript'):
            new_subscripts = self.transform_all_subnodes(
                subscript, warn=False, ignored={'section-subscript'})
            if not new_subscripts:
                continue
            if len(new_subscripts) == 1:
                new_subscript = typed_ast3.Index(value=new_subscripts[0])
            elif len(new_subscripts) == 2:
                new_subscript = typed_ast3.Slice(
                    lower=new_subscripts[0], upper=new_subscripts[1], step=None)
            else:
                _LOG.error('%s', ET.tostring(subscript).decode().rstrip())
                _LOG.error('%s', [typed_astunparse.unparse(_) for _ in new_subscripts])
                raise SyntaxError('there must be 1 or 2 new subscript data elements')
            subscripts.append(new_subscript)
        if len(subscripts) == 1:
            return subscripts[0]
        elif len(subscripts) > 1:
            return typed_ast3.ExtSlice(dims=subscripts)
        return [] # TODO: TMP
        raise SyntaxError(
            'subscripts node must contain at least one "subscript" node:\n{}'
            .format(ET.tostring(node).decode().rstrip()))

    def _literal(self, node: ET.Element) -> t.Union[typed_ast3.Num, typed_ast3.Str]:
        literal_type = node.attrib['type']
        if literal_type == 'bool':
            return typed_ast3.NameConstant(value={
                'false': False,
                'true': True}[node.attrib['value']])
        if literal_type == 'int':
            return typed_ast3.Num(n=int(node.attrib['value']))
        if literal_type == 'real':
            value = node.attrib['value']
            if 'D' in value:
                value = value.replace('D', 'E', 1)
            return typed_ast3.Num(n=float(value))
        if literal_type == 'char':
            assert len(node.attrib['value']) >= 2
            begin = node.attrib['value'][0]
            end = node.attrib['value'][-1]
            assert begin == end
            return typed_ast3.Str(s=node.attrib['value'][1:-1])
        _LOG.warning('%s', ET.tostring(node).decode().rstrip())
        raise NotImplementedError('literal type "{}" not supported'.format(literal_type))


def transform(root_node: ET.Element) -> typed_ast3.AST:
    file_node = root_node[0]
    transformer = AstTransformer()
    return transformer.transform(file_node)
