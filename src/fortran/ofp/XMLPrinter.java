package fortran.ofp;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.antlr.runtime.Token;
import org.apache.commons.cli.CommandLine;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import fortran.ofp.parser.java.FortranParserActionPrint;
import fortran.ofp.parser.java.IFortranParser;

/**
 * XML output generator for Open Fortran Parser.
 *
 * @author Mateusz Bysiek https://mbdevpl.github.io/
 */
public class XMLPrinter extends FortranParserActionPrint {

	/**
	 * Parsed command-line arguments.
	 */
	CommandLine cmd;

	/**
	 * Verbosity level from 0 to 100.
	 */
	int verbosity;

	/**
	 * XML document.
	 */
	protected Document doc;

	/**
	 * XML root node, the outermost open XML context.
	 */
	protected Element root;

	/**
	 * Current open XML context.
	 */
	protected Element context = null;

	public XMLPrinter(String[] args, IFortranParser parser, String filename) {
		super(args, parser, filename);

		// System.err.println(Arrays.toString(args));
		cmd = new ArgsParser().parse(args);
		verbosity = Integer.parseInt(cmd.getOptionValue("verbosity", "100"));
		if (verbosity >= 100) {
			setVerbose(true);
			setPrintKeywords(true);
		}

		try {
			DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
			DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
			doc = docBuilder.newDocument();
			root = contextOpen("ofp");
			setAttribute("version", "0.8.4");
			doc.appendChild(root);
		} catch (Exception error) {
			error.printStackTrace();
			System.exit(1);
		}
	}

	/**
	 * Open a new XML context.
	 *
	 * @param name
	 */
	protected Element contextOpen(String name) {
		Element new_context = doc.createElement(name);
		if (context != null)
			context.appendChild(new_context);
		context = new_context;
		return context;
	}

	/**
	 * Find innermost open XML context with name equal to any of given names.
	 *
	 * @param names
	 * @return
	 */
	protected Element contextFind(String... names) {
		if (context == null)
			throw new NullPointerException("No open contexts, so " + Arrays.toString(names) + " cannot be found.");
		Element found = context;
		List<String> names_list = Arrays.asList(names);
		while (!names_list.contains(found.getTagName())) {
			if (found == root) {
				System.err.println("Cannot find any context of " + Arrays.toString(names) + " among open contexts.");
				System.err.println("Current context hierarchy (innermost first) is:");
				found = context;
				while (found != root) {
					System.err.println("  " + found.getTagName());
					found = (Element) found.getParentNode();
				}
				cleanUp();
				System.exit(1);
			}
			found = (Element) found.getParentNode();
		}
		return found;
	}

	/**
	 * Rename first innermost context with name equal given name.
	 *
	 * @param fromName
	 * @param toName
	 */
	protected void contextRename(String fromName, String toName) {
		if (context.getTagName() != fromName) {
			System.err.println("Cannot rename current context from '" + fromName + "' to '" + toName
					+ "' because it has unexpected name '" + context.getTagName() + "'.");
			cleanUp();
			System.exit(1);
		}
		doc.renameNode(context, null, toName);
	}

	/**
	 * Close the current XML context.
	 *
	 * @param name
	 */
	protected void contextClose() {
		if (context == null || context == root) {
			if (context == null)
				System.err.println("Cannot close current context because there is no current context.");
			else
				System.err.println("Cannot close current context because it is root node of the document.");
			cleanUp();
			System.exit(1);
		}
		context = (Element) context.getParentNode();
	}

	/**
	 * Close all inner open XML contexts (if any) that are in first innermost context with name equal to any of given
	 * names.
	 *
	 * @param name
	 */
	protected void contextCloseAllInner(String... names) {
		context = (Element) contextFind(names);
	}

	/**
	 * Close an innermost open XML context with name equal to a given name.
	 *
	 * @param name
	 */
	protected void contextClose(String name) {
		contextCloseAny(name);
	}

	/**
	 * Close an innermost open XML context with name equal to any of given names.
	 *
	 * @param names
	 */
	protected void contextCloseAny(String... names) {
		contextCloseAllInner(names);
		contextClose();
	}

	/**
	 * Collection of children nodes of current XML context.
	 *
	 * @return
	 */
	protected ArrayList<Node> contextNodes() {
		return contextNodes(null);
	}

	/**
	 * Collection of children nodes of given open XML context.
	 *
	 * @param name
	 * @return
	 */
	protected ArrayList<Node> contextNodes(String name) {
		Element context;
		if (name == null)
			context = this.context;
		else
			context = contextFind(name);
		NodeList nodeList = context.getChildNodes();
		ArrayList<Node> nodes = new ArrayList<Node>();
		for (int i = 0; i < nodeList.getLength(); i++)
			nodes.add(nodeList.item(i));
		// System.err.println(nodes.size());
		return nodes;
	}

	/**
	 * Set attribute for a current XML context.
	 *
	 * @param name
	 * @param value
	 */
	protected void setAttribute(String name, Object value) {
		setAttribute(name, value, null);
	}

	/**
	 * Set attribute for a given open context.
	 *
	 * @param name
	 * @param value
	 * @param contextName
	 */
	protected void setAttribute(String name, Object value, String contextName) {
		/*
		if (value == null)
			return;
		*/

		Element on_context = null;
		if (contextName == null)
			on_context = context;
		else
			on_context = contextFind(contextName);

		String valueString = null;
		if (value == null)
			valueString = "";
		else if (value instanceof Token)
			valueString = ((Token) value).getText();
		else
			valueString = value.toString();
		on_context.setAttribute(name, valueString);
	}

	protected void printRuleHeader(int rule, String name, String addendum) {
		contextOpen(name);
		setAttribute("rule", rule);
		if (addendum.length() > 0)
			setAttribute("addendum", addendum);
	}

	protected void printRuleTrailer() {
		contextClose();
	}

	protected void printParameter(Object param, String name) {
		setAttribute(name, param);
	}

	protected void printParameter(Token param, String name) {
		setAttribute(name, param);
	}

	@Override
	public void generic_name_list__begin() {
		if (context.getTagName() == "specification") {
			contextOpen("declaration");
		}
		contextOpen("variables");
		if (verbosity >= 100)
			super.generic_name_list__begin();
	}

	@Override
	public void generic_name_list(int count) {
		if (verbosity >= 100)
			super.generic_name_list(count);
		setAttribute("count", count);
		contextClose("variables");
	}

	public void specification_part(int numUseStmts, int numImportStmts, int numImplStmts, int numDeclConstructs) {
		if (verbosity >= 80)
			super.specification_part(numUseStmts, numImportStmts, numImplStmts, numDeclConstructs);
		setAttribute("uses", numUseStmts);
		setAttribute("imports", numImportStmts);
		setAttribute("implicits", numImplStmts);
		setAttribute("declarations", numDeclConstructs);
		contextClose("specification");
		contextOpen("statement");
	}

	public void declaration_construct() {
		if (verbosity >= 100)
			super.declaration_construct();
		contextClose("declaration");
	}

	public void execution_part_construct() {
		if (verbosity >= 100)
			super.execution_part_construct();
	}

	public void specification_stmt() {
		if (verbosity >= 100)
			super.specification_stmt();
	}

	public void executable_construct() {
		if (verbosity >= 100)
			super.executable_construct();
	}

	public void action_stmt() {
		if (verbosity >= 100)
			super.action_stmt();
		contextClose("statement");
		contextOpen("statement");
	}

	public void keyword() {
		if (verbosity >= 100)
			super.keyword();
	}

	public void name(Token id) {
		super.name(id);
	}

	public void constant(Token id) {
		super.constant(id);
	}

	public void scalar_constant() {
		if (verbosity >= 100)
			super.scalar_constant();
	}

	public void literal_constant() {
		if (verbosity >= 100)
			super.literal_constant();
		contextClose("literal");
	}

	public void intrinsic_type_spec(Token keyword1, Token keyword2, int type, boolean hasKindSelector) {
		contextOpen("declaration");
		setAttribute("type", "variable");
		super.intrinsic_type_spec(keyword1, keyword2, type, hasKindSelector);
		// contextOpen("intrinsic-type");
	}

	public void int_literal_constant(Token digitString, Token kindParam) {
		contextOpen("literal");
		setAttribute("type", "int");
		setAttribute("value", digitString);
		super.int_literal_constant(digitString, kindParam);
	}

	public void scalar_int_literal_constant() {
		if (verbosity >= 100)
			super.scalar_int_literal_constant();
		contextClose("literal");
	}

	public void char_literal_constant(Token digitString, Token id, Token str) {
		contextOpen("literal");
		setAttribute("type", "char");
		setAttribute("value", str);
		super.char_literal_constant(digitString, id, str);
	}

	public void type_declaration_stmt(Token label, int numAttributes, Token eos) {
		super.type_declaration_stmt(label, numAttributes, eos);
		// contextClose("intrinsic-type");
	}

	public void entity_decl(Token id, boolean hasArraySpec, boolean hasCoarraySpec, boolean hasCharLength,
			boolean hasInitialization) {
		/*
		if (context.getTagName() != "entry") {
			contextOpen("entry");
			setAttribute("type", "scalar");
		}
		*/
		super.entity_decl(id, hasArraySpec, hasCoarraySpec, hasCharLength, hasInitialization);
		// contextClose("entry");
	}

	public void entity_decl_list__begin() {
		contextOpen("variables");
		if (verbosity >= 100)
			super.entity_decl_list__begin();
	}

	public void entity_decl_list(int count) {
		if (verbosity >= 100)
			super.entity_decl_list(count);
		setAttribute("count", count);
		contextClose("variables");
	}

	public void access_spec(Token keyword, int type) {
		contextOpen("declaration");
		super.access_spec(keyword, type);
	}

	public void array_spec_element(int type) {
		/*
		if (context.getTagName() != "entry") {
			contextOpen("entry");
			setAttribute("type", "array");
		}
		*/
		super.array_spec_element(type);
	}

	public void access_id_list__begin() {
		// contextOpen("access-list");
		if (verbosity >= 100)
			super.access_id_list__begin();
	}

	public void access_id_list(int count) {
		super.access_id_list(count);
		// contextClose("access-list");
	}

	public void data_stmt_object_list__begin() {
		contextOpen("declaration");
		setAttribute("type", "data");
		contextOpen("variables");
		if (verbosity >= 100)
			super.data_stmt_object_list__begin();
	}

	public void data_stmt_object_list(int count) {
		if (verbosity >= 100)
			super.data_stmt_object_list(count);
		setAttribute("count", count);
		contextClose("variables");
	}

	public void data_stmt_value_list__begin() {
		contextOpen("values");
		if (verbosity >= 100)
			super.data_stmt_value_list__begin();
	}

	public void data_stmt_value_list(int count) {
		if (verbosity >= 100)
			super.data_stmt_value_list(count);
		setAttribute("count", count);
		contextClose("values");
	}

	public void named_constant_def_list__begin() {
		contextOpen("declaration");
		setAttribute("type", "parameter");
		if (verbosity >= 100)
			super.named_constant_def_list__begin();
	}

	public void implicit_stmt(Token label, Token implicitKeyword, Token noneKeyword, Token eos,
			boolean hasImplicitSpecList) {
		contextOpen("declaration");
		if (verbosity >= 20)
			super.implicit_stmt(label, implicitKeyword, noneKeyword, eos, hasImplicitSpecList);
		setAttribute("type", "implicit");
		setAttribute("subtype", noneKeyword == null ? "some" : "none");
		contextClose();
	}

	public void variable() {
		if (verbosity >= 100)
			super.variable();
		setAttribute("type", "variable");
		contextClose("name");
	}

	public void designator_or_func_ref() {
		if (verbosity >= 100)
			super.designator_or_func_ref();
		setAttribute("type", "ambiguous");
		contextClose("name");
	}

	public void part_ref(Token id, boolean hasSectionSubscriptList, boolean hasImageSelector) {
		Element outer_context = context;
		Element e = null;
		if (hasSectionSubscriptList) {
			ArrayList<Node> nodes = contextNodes();
			e = (Element) nodes.get(nodes.size() - 1);
			if (e.getTagName() != "subscripts") {
				cleanUp();
				System.exit(1);
			}
		}
		contextOpen("name");
		setAttribute("id", id);
		if (hasSectionSubscriptList) {
			outer_context.removeChild(e);
			context.appendChild(e);
		}
		if (verbosity >= 60)
			super.part_ref(id, hasSectionSubscriptList, hasImageSelector);
	}

	public void section_subscript(boolean hasLowerBound, boolean hasUpperBound, boolean hasStride,
			boolean isAmbiguous) {
		if (verbosity >= 80)
			super.section_subscript(hasLowerBound, hasUpperBound, hasStride, isAmbiguous);
		contextClose("subscript");
		contextOpen("subscript");
	}

	public void section_subscript_list__begin() {
		contextOpen("subscripts");
		if (verbosity >= 100)
			super.section_subscript_list__begin();
		contextOpen("subscript");
	}

	public void section_subscript_list(int count) {
		contextClose("subscript");
		if (verbosity >= 100)
			super.section_subscript_list(count);
		setAttribute("count", count);
		contextClose("subscripts");
	}

	public void primary() {
		if (verbosity >= 100)
			super.primary();
		// contextCloseAny("literal", "name");
	}

	public void parenthesized_expr() {
		if (verbosity >= 100)
			super.parenthesized_expr();
	}

	public void level_3_expr(Token relOp) {
		if (verbosity >= 80)
			super.level_3_expr(relOp);
		if (relOp == null)
			return;
		contextClose("operand");
		setAttribute("operator", relOp);
		contextClose("operation");
	}

	public void rel_op(Token relOp) {
		ArrayList<Node> nodes = contextNodes();
		Element outer_context = context;
		contextOpen("operation");
		contextOpen("operand");
		for (Node node : nodes) {
			outer_context.removeChild(node);
			context.appendChild(node);
		}
		if (verbosity >= 100)
			super.rel_op(relOp);
		contextClose("operand");
		setAttribute("type", "binary");
		contextOpen("operand");
	}

	public void block() {
		contextCloseAllInner("body");
		if (verbosity >= 100)
			super.block();
	}

	public void if_construct() {
		contextCloseAllInner("if");
		if (verbosity >= 100)
			super.if_construct();
		contextClose("if");
		contextOpen("statement");
	}

	public void if_then_stmt(Token label, Token id, Token ifKeyword, Token thenKeyword, Token eos) {
		contextRename("statement", "if");
		ArrayList<Node> nodes = contextNodes();
		Element outer_context = context;
		contextOpen("header");
		for (Node node : nodes) {
			// System.err.println(" " + ((Element) node).getTagName());
			outer_context.removeChild(node);
			context.appendChild(node);
		}
		contextClose("header");
		if (verbosity >= 80)
			super.if_then_stmt(label, id, ifKeyword, thenKeyword, eos);
		contextOpen("body");
		contextOpen("statement");
	}

	public void else_if_stmt(Token label, Token elseKeyword, Token ifKeyword, Token thenKeyword, Token id, Token eos) {
		super.else_if_stmt(label, elseKeyword, ifKeyword, thenKeyword, id, eos);
	}

	public void else_stmt(Token label, Token elseKeyword, Token id, Token eos) {
		super.else_stmt(label, elseKeyword, id, eos);
	}

	public void end_if_stmt(Token label, Token endKeyword, Token ifKeyword, Token id, Token eos) {
		contextCloseAllInner("if");
		if (verbosity >= 80)
			super.end_if_stmt(label, endKeyword, ifKeyword, id, eos);
	}

	public void if_stmt__begin() {
		if (verbosity >= 100)
			super.if_stmt__begin();
	}

	public void if_stmt(Token label, Token ifKeyword) {
		super.if_stmt(label, ifKeyword);
	}

	public void block_construct() {
		if (verbosity >= 100)
			super.block_construct();
	}

	public void do_construct() {
		contextCloseAllInner("loop");
		if (verbosity >= 100)
			super.do_construct();
		contextClose("loop");
		contextOpen("statement");
	}

	public void block_do_construct() {
		if (verbosity >= 100)
			super.block_do_construct();
	}

	public void do_stmt(Token label, Token id, Token doKeyword, Token digitString, Token eos, boolean hasLoopControl) {
		contextClose("header");
		super.do_stmt(label, id, doKeyword, digitString, eos, hasLoopControl);
		contextOpen("body");
		contextOpen("statement");
	}

	public void label_do_stmt(Token label, Token id, Token doKeyword, Token digitString, Token eos,
			boolean hasLoopControl) {
		contextClose("header");
		super.label_do_stmt(label, id, doKeyword, digitString, eos, hasLoopControl);
		contextOpen("body");
		contextOpen("statement");
	}

	public void loop_control(Token whileKeyword, int doConstructType, boolean hasOptExpr) {
		super.loop_control(whileKeyword, doConstructType, hasOptExpr);
		setAttribute("subtype", doConstructType, "loop");
	}

	public void do_variable(Token id) {
		contextRename("statement", "loop");
		setAttribute("type", "do");
		contextOpen("header");
		super.do_variable(id);
	}

	public void end_do() {
		if (verbosity >= 100)
			super.end_do();
	}

	public void end_do_stmt(Token label, Token endKeyword, Token doKeyword, Token id, Token eos) {
		contextCloseAllInner("loop");
		if (verbosity >= 80)
			super.end_do_stmt(label, endKeyword, doKeyword, id, eos);
	}

	@Override
	public void input_item() {
		contextClose("input");
		if (verbosity >= 100)
			super.input_item();
		contextOpen("input");
	}

	@Override
	public void input_item_list__begin() {
		contextOpen("inputs");
		if (verbosity >= 100)
			super.input_item_list__begin();
		contextOpen("input");
	}

	@Override
	public void input_item_list(int count) {
		contextClose("input");
		if (verbosity >= 100)
			super.input_item_list(count);
		setAttribute("count", count);
		contextClose("inputs");
	}

	@Override
	public void output_item() {
		contextClose("output");
		if (verbosity >= 100)
			super.output_item();
		contextOpen("output");
	}

	@Override
	public void output_item_list__begin() {
		contextOpen("outputs");
		if (verbosity >= 100)
			super.output_item_list__begin();
		contextOpen("output");
	}

	@Override
	public void output_item_list(int count) {
		contextClose("output");
		if (verbosity >= 100)
			super.output_item_list(count);
		setAttribute("count", count);
		contextClose("outputs");
	}

	public void main_program__begin() {
		contextOpen("program");
		contextOpen("header");
		if (verbosity >= 100)
			super.main_program__begin();
	}

	public void ext_function_subprogram(boolean hasPrefix) {
		super.ext_function_subprogram(hasPrefix);
		contextClose("function");
	}

	public void main_program(boolean hasProgramStmt, boolean hasExecutionPart, boolean hasInternalSubprogramPart) {
		super.main_program(hasProgramStmt, hasExecutionPart, hasInternalSubprogramPart);
		contextClose("program");
	}

	public void program_stmt(Token label, Token programKeyword, Token id, Token eos) {
		contextClose("header");
		if (verbosity >= 20)
			super.program_stmt(label, programKeyword, id, eos);
		setAttribute("name", id);
		contextOpen("body");
		contextOpen("specification");
	}

	public void end_program_stmt(Token label, Token endKeyword, Token programKeyword, Token id, Token eos) {
		contextCloseAllInner("program");
		super.end_program_stmt(label, endKeyword, programKeyword, id, eos);
	}

	public void module() {
		if (verbosity >= 100)
			super.module();
		contextClose("module");
	}

	public void module_stmt__begin() {
		contextOpen("module");
		if (verbosity >= 100)
			super.module_stmt__begin();
	}

	public void use_stmt(Token label, Token useKeyword, Token id, Token onlyKeyword, Token eos, boolean hasModuleNature,
			boolean hasRenameList, boolean hasOnly) {
		if (context.getTagName() != "use") {
			contextOpen("use");
		}
		super.use_stmt(label, useKeyword, id, onlyKeyword, eos, hasModuleNature, hasRenameList, hasOnly);
		contextClose("use");
	}

	public void rename_list__begin() {
		contextOpen("use");
		contextOpen("rename");
		if (verbosity >= 100)
			super.rename_list__begin();
	}

	public void rename_list(int count) {
		super.rename_list(count);
		contextClose("rename");
	}

	public void only_list__begin() {
		contextOpen("use");
		contextOpen("only");
		if (verbosity >= 100)
			super.only_list__begin();
	}

	public void only_list(int count) {
		super.only_list(count);
		contextClose("only");
	}

	public void block_data() {
		if (verbosity >= 100)
			super.block_data();
		contextClose("block-data");
	}

	public void block_data_stmt__begin() {
		contextOpen("block-data");
		if (verbosity >= 100)
			super.block_data_stmt__begin();
		contextOpen("specification");
	}

	public void external_stmt(Token label, Token externalKeyword, Token eos) {
		if (verbosity >= 80)
			super.external_stmt(label, externalKeyword, eos);
		setAttribute("type", "external");
	}

	public void call_stmt(Token label, Token callKeyword, Token eos, boolean hasActualArgSpecList) {
		contextOpen("call");
		super.call_stmt(label, callKeyword, eos, hasActualArgSpecList);
		contextClose("call");
	}

	public void procedure_designator() {
		if (verbosity >= 100)
			super.procedure_designator();
		setAttribute("type", "procedure");
		contextClose("name");
	}

	public void function_stmt__begin() {
		contextOpen("function");
		contextOpen("header");
		if (verbosity >= 100)
			super.function_stmt__begin();
	}

	public void function_stmt(Token label, Token keyword, Token name, Token eos, boolean hasGenericNameList,
			boolean hasSuffix) {
		contextClose("header");
		super.function_stmt(label, keyword, name, eos, hasGenericNameList, hasSuffix);
		setAttribute("name", name);
		contextOpen("body");
		contextOpen("specification");
	}

	public void end_function_stmt(Token label, Token keyword1, Token keyword2, Token name, Token eos) {
		contextCloseAllInner("function");
		super.end_function_stmt(label, keyword1, keyword2, name, eos);
	}

	public void subroutine_stmt__begin() {
		contextOpen("subroutine");
		contextOpen("header");
		if (verbosity >= 100)
			super.subroutine_stmt__begin();
	}

	public void subroutine_stmt(Token label, Token keyword, Token name, Token eos, boolean hasPrefix,
			boolean hasDummyArgList, boolean hasBindingSpec, boolean hasArgSpecifier) {
		super.subroutine_stmt(label, keyword, name, eos, hasPrefix, hasDummyArgList, hasBindingSpec, hasArgSpecifier);
		contextClose("header");
		setAttribute("name", name);
		contextOpen("body");
		contextOpen("specification");
	}

	public void dummy_arg_list__begin() {
		contextOpen("arguments");
		if (verbosity >= 100)
			super.dummy_arg_list__begin();
	}

	public void dummy_arg_list(int count) {
		super.dummy_arg_list(count);
		contextClose("arguments");
	}

	public void end_subroutine_stmt(Token label, Token keyword1, Token keyword2, Token name, Token eos) {
		contextCloseAllInner("subroutine");
		super.end_subroutine_stmt(label, keyword1, keyword2, name, eos);
		contextClose("subroutine");
	}

	public void contains_stmt(Token label, Token keyword, Token eos) {
		// contextOpen("contains"); // TODO: is it needed?
		super.contains_stmt(label, keyword, eos);
	}

	public void start_of_file(String filename, String path) {
		contextOpen("file");
		if (verbosity >= 100)
			super.start_of_file(filename, path);
		setAttribute("path", path);
	}

	public void end_of_file(String filename, String path) {
		contextCloseAllInner("file");
		if (verbosity >= 100)
			super.end_of_file(filename, path);
	}

	public void cleanUp() {
		while (context != root)
			contextClose();
		try {
			TransformerFactory transformerFactory = TransformerFactory.newInstance();
			Transformer transformer = transformerFactory.newTransformer();
			transformer.setOutputProperty(OutputKeys.INDENT, "yes");
			transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");
			DOMSource source = new DOMSource(doc);
			StreamResult result;
			if (cmd.hasOption("output"))
				result = new StreamResult(new File(cmd.getOptionValue("output")));
			else
				result = new StreamResult(System.out);
			transformer.transform(source, result);
		} catch (Exception error) {
			error.printStackTrace();
			System.exit(1);
		}
	}

	public void next_token(Token tk) {
		System.err.println("next_token");
		System.err.println(tk);
	}

}
