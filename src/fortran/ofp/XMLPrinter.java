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
	 * Try to find innermost open XML context with name equal to any of given names.
	 *
	 * @param names
	 * @return found context of null
	 */
	protected Element contextTryFind(String... names) {
		if (context == null)
			return null;
		Element found = context;
		List<String> names_list = Arrays.asList(names);
		while (!names_list.contains(found.getTagName())) {
			if (found == root)
				return null;
			found = (Element) found.getParentNode();
		}
		return found;
	}

	protected Element contextFind(String... names) {
		if (context == null)
			throw new NullPointerException("No open contexts, so " + Arrays.toString(names) + " cannot be found.");
		Element found = contextTryFind(names);
		if (found != null)
			return found;
		System.err.println("Cannot find any context of " + Arrays.toString(names) + " among open contexts.");
		System.err.println("Current context hierarchy (innermost first) is:");
		found = context;
		while (found != root) {
			System.err.println("  " + found.getTagName());
			found = (Element) found.getParentNode();
		}
		cleanUpAfterError();
		return null;
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
			cleanUpAfterError();
		}
		doc.renameNode(context, null, toName);
	}

	/**
	 * Close the given XML context.
	 *
	 * @param name
	 */
	protected void contextClose(Element context) {
		if (context == root) {
			// if (context == null)
			// System.err.println("Cannot close given context because 'null' was given.");
			// else
			System.err.println("Cannot close given context because it is root node of the document.");
			cleanUpAfterError();
		}
		this.context = (Element) context.getParentNode();
	}

	protected void contextClose() {
		contextClose(context);
	}

	/**
	 * Close an innermost open XML context with name equal to any of given names.
	 *
	 * @param names
	 */
	protected void contextClose(String... names) {
		contextClose(contextFind(names));
	}

	/**
	 * Close all inner open XML contexts (if any) that are in first innermost context with name equal to any of given
	 * names.
	 *
	 * @param name
	 */
	protected void contextCloseAllInner(String... names) {
		context = contextFind(names);
	}

	/**
	 * Collection of children nodes of given XML context.
	 *
	 * @param context
	 * @param begin_index the index will be chosen from the end if negative number is given
	 * @param count number of results to return, return all results if zero is given
	 * @return list of nodes
	 */
	protected ArrayList<Element> contextNodes(Element context, int beginIndex, int count) {
		NodeList nodeList = context.getChildNodes();
		ArrayList<Element> nodes = new ArrayList<Element>();
		// System.err.println("contextNodes of " + context + " " + beginIndex + " " + count);
		if (beginIndex < 0)
			beginIndex = nodeList.getLength() + beginIndex;
		if (beginIndex < 0 || beginIndex >= nodeList.getLength())
			throw new IndexOutOfBoundsException(
					"starting index " + beginIndex + " out of bounds [" + 0 + ", " + nodeList.getLength() + ")");
		if (count == 0)
			count = nodeList.getLength() - beginIndex;
		if (count < 0)
			throw new IndexOutOfBoundsException("attemted to return " + count + " number of nodes");
		int endIndex = beginIndex + count;
		/*
		System.err.println("returning " + count + " subnodes of " + context + " (from index " + beginIndex + " to "
				+ endIndex + ")");
		*/
		for (int i = beginIndex; i < endIndex; i++)
			nodes.add((Element) nodeList.item(i));
		return nodes;
	}

	protected ArrayList<Element> contextNodes(Element context) {
		return contextNodes(context, 0, 0);
	}

	protected ArrayList<Element> contextNodes(int beginIndex, int count) {
		return contextNodes(context, beginIndex, count);
	}

	protected ArrayList<Element> contextNodes() {
		return contextNodes(context, 0, 0);
	}

	protected Element contextNode(Element context, int index) {
		return contextNodes(context, index, 1).get(0);
	}

	protected Element contextNode(int index) {
		return contextNode(context, index);
	}

	/**
	 * Set attribute for a given context.
	 *
	 * @param name
	 * @param value
	 * @param contextName
	 */
	protected void setAttribute(String name, Object value, Element context) {
		String valueString = null;
		if (value == null)
			valueString = "";
		else if (value instanceof Token)
			valueString = ((Token) value).getText();
		else
			valueString = value.toString();
		context.setAttribute(name, valueString);
	}

	protected void setAttribute(String name, Object value, String... names) {
		setAttribute(name, value, contextFind(names));
	}

	protected void setAttribute(String name, Object value) {
		setAttribute(name, value, context);
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

	public void generic_name_list_part(Token id) {
		contextOpen("name");
		setAttribute("id", id);
		if (verbosity >= 100)
			super.generic_name_list_part(id);
		contextClose("name");
	}

	public void generic_name_list__begin() {
		if (context.getTagName().equals("specification") || context.getTagName().equals("file"))
			contextOpen("declaration");
		contextOpen("names");
		if (verbosity >= 100)
			super.generic_name_list__begin();
	}

	public void generic_name_list(int count) {
		if (verbosity >= 100)
			super.generic_name_list(count);
		setAttribute("count", count);
		contextClose("names");
	}

	public void specification_part(int numUseStmts, int numImportStmts, int numImplStmts, int numDeclConstructs) {
		if (context.getTagName().equals("header")) {
			contextClose("header");
			contextOpen("body");
		}
		if (context.getTagName().equals("declaration"))
			contextClose("declaration");
		if (!context.getTagName().equals("specification"))
			contextOpen("specification");
		contextCloseAllInner("specification");
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
		contextClose("declaration");
		if (verbosity >= 100)
			super.declaration_construct();
		contextOpen("declaration");
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

	public void label(Token lbl) {
		boolean closedLoop = false;
		Element outerContext = context;
		while (outerContext != root) {
			if (outerContext.getTagName().equals("loop") && outerContext.getAttribute("label").equals(lbl.getText())) {
				context = outerContext;
				closedLoop = true;
				break;
			}
			outerContext = (Element) outerContext.getParentNode();
		}
		super.label(lbl);
		if (closedLoop)
			contextOpen("statement");
	}

	public void intrinsic_type_spec(Token keyword1, Token keyword2, int type, boolean hasKindSelector) {
		if (!context.getTagName().equals("declaration")) {
			// TODO: ensure being in body
			contextOpen("declaration");
		}
		setAttribute("type", "variable");
		super.intrinsic_type_spec(keyword1, keyword2, type, hasKindSelector);
	}

	public void int_literal_constant(Token digitString, Token kindParam) {
		contextOpen("literal");
		setAttribute("type", "int");
		setAttribute("value", digitString);
		super.int_literal_constant(digitString, kindParam);
	}

	public void boz_literal_constant(Token constant) {
		contextOpen("literal");
		setAttribute("type", "int");
		setAttribute("value", constant);
		super.boz_literal_constant(constant);
	}

	public void real_literal_constant(Token realConstant, Token kindParam) {
		contextOpen("literal");
		setAttribute("type", "real");
		setAttribute("value", realConstant);
		super.real_literal_constant(realConstant, kindParam);
	}

	/*
	public void char_length(boolean hasTypeParamValue) {
		Element outerContext = context;
		Element value = contextNode(-1);
		contextOpen("length");
		outerContext.removeChild(value);
		context.appendChild(value);
		super.char_length(hasTypeParamValue);
		contextClose("length");
	}
	*/

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

	public void logical_literal_constant(Token logicalValue, boolean isTrue, Token kindParam) {
		contextOpen("literal");
		setAttribute("type", "bool");
		setAttribute("value", isTrue);
		super.logical_literal_constant(logicalValue, isTrue, kindParam);
	}

	public void derived_type_stmt(Token label, Token keyword, Token id, Token eos, boolean hasTypeAttrSpecList,
			boolean hasGenericNameList) {
		if (!context.getTagName().equals("declaration"))
			contextOpen("declaration");
		setAttribute("type", "type");
		super.derived_type_stmt(label, keyword, id, eos, hasTypeAttrSpecList, hasGenericNameList);
	}

	public void derived_type_spec(Token typeName, boolean hasTypeParamSpecList) {
		if (!context.getTagName().equals("declaration"))
			contextOpen("declaration");
		setAttribute("type", "variable");
		super.derived_type_spec(typeName, hasTypeParamSpecList);
	}

	public void ac_value() {
		contextClose("value");
		if (verbosity >= 100)
			super.ac_value();
		contextOpen("value");
	}

	public void ac_value_list__begin() {
		contextOpen("array-constructor-values");
		if (verbosity >= 100)
			super.ac_value_list__begin();
		contextOpen("value");
	}

	public void ac_value_list(int count) {
		contextClose("value");
		contextCloseAllInner("array-constructor-values");
		setAttribute("count", count);
		if (verbosity >= 100)
			super.ac_value_list(count);
		contextClose("array-constructor-values");
	}

	public void type_declaration_stmt(Token label, int numAttributes, Token eos) {
		super.type_declaration_stmt(label, numAttributes, eos);
	}

	public void declaration_type_spec(Token udtKeyword, int type) {
		Element outerContext = context;
		ArrayList<Element> typeDeclarations = contextNodes();
		contextOpen("type");
		setAttribute("hasLength", false);
		setAttribute("hasKind", false);
		setAttribute("hasDimensions", false);
		for (Element declaration : typeDeclarations) {
			outerContext.removeChild(declaration);
			switch (declaration.getTagName()) {
			case "intrinsic-type-spec":
				setAttribute("name", declaration.getAttribute("keyword1"));
				break;
			case "length":
				setAttribute("hasLength", true);
				break;
			case "kind":
				setAttribute("hasKind", true);
				break;
			case "dimensions":
				setAttribute("hasDimensions", true);
				break;
			default:
				break;
			}
			context.appendChild(declaration);
		}
		super.declaration_type_spec(udtKeyword, type);
		contextClose("type");
	}

	public void entity_decl(Token id, boolean hasArraySpec, boolean hasCoarraySpec, boolean hasCharLength,
			boolean hasInitialization) {
		contextCloseAllInner("variable");
		super.entity_decl(id, hasArraySpec, hasCoarraySpec, hasCharLength, hasInitialization);
		setAttribute("name", id);
		setAttribute("hasInitialValue", hasInitialization);
		contextClose("variable");
		contextOpen("variable");
	}

	public void entity_decl_list__begin() {
		contextOpen("variables");
		if (verbosity >= 100)
			super.entity_decl_list__begin();
		contextOpen("variable");
	}

	public void entity_decl_list(int count) {
		contextClose("variable");
		contextCloseAllInner("variables");
		if (verbosity >= 100)
			super.entity_decl_list(count);
		setAttribute("count", count);
		contextClose("variables");
	}

	public void initialization(boolean hasExpr, boolean hasNullInit) {
		Element outerContext = context;
		Element initialValue = contextNode(-1);
		contextOpen("initial-value");
		outerContext.removeChild(initialValue);
		context.appendChild(initialValue);
		super.initialization(hasExpr, hasNullInit);
		contextClose("initial-value");
	}

	public void access_spec(Token keyword, int type) {
		if (!context.getTagName().equals("declaration"))
			contextOpen("declaration");
		super.access_spec(keyword, type);
	}

	public void language_binding_spec(Token keyword, Token id, boolean hasName) {
		if (!context.getTagName().equals("declaration"))
			contextOpen("declaration");
		super.language_binding_spec(keyword, id, hasName);
	}

	/*
	public void array_spec(int count) {
		contextCloseAllInner("dimensions");
		if (verbosity >= 100)
			super.array_spec(count);
		setAttribute("count", count);
		contextClose("dimensions");
	}

	public void array_spec_element(int type) {
		Element outerContext = context;
		Element value = contextNode(-1);
		if (!context.getTagName().equals("dimensions"))
			contextOpen("dimensions");
		contextOpen("dimension");
		outerContext.removeChild(value);
		context.appendChild(value);
		super.array_spec_element(type);
		contextClose("dimension");
	}
	*/

	public void access_id_list__begin() {
		// contextOpen("access-list");
		if (verbosity >= 100)
			super.access_id_list__begin();
	}

	public void access_id_list(int count) {
		super.access_id_list(count);
		// contextClose("access-list");
	}

	public void allocatable_decl_list__begin() {
		if (!context.getTagName().equals("declaration"))
			contextOpen("declaration");
		setAttribute("type", "allocatables");
		super.allocatable_decl_list__begin();
	}

	public void codimension_decl_list__begin() {
		if (!context.getTagName().equals("declaration"))
			contextOpen("declaration");
		setAttribute("type", "codimensions");
		super.codimension_decl_list__begin();
	}

	public void data_stmt_object_list__begin() {
		if (!context.getTagName().equals("declaration"))
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

	public void hollerith_literal_constant(Token hollerithConstant) {
		contextOpen("literal");
		setAttribute("type", "hollerith");
		setAttribute("value", hollerithConstant);
		super.hollerith_literal_constant(hollerithConstant);
	}

	public void named_constant_def_list__begin() {
		if (!context.getTagName().equals("declaration"))
			contextOpen("declaration");
		setAttribute("type", "parameter");
		if (verbosity >= 100)
			super.named_constant_def_list__begin();
	}

	public void pointer_decl_list__begin() {
		if (!context.getTagName().equals("declaration"))
			contextOpen("declaration");
		super.pointer_decl_list__begin();
	}

	public void save_stmt(Token label, Token keyword, Token eos, boolean hasSavedEntityList) {
		if (!context.getTagName().equals("declaration"))
			contextOpen("declaration");
		super.save_stmt(label, keyword, eos, hasSavedEntityList);
	}

	public void target_decl_list__begin() {
		if (!context.getTagName().equals("declaration"))
			contextOpen("declaration");
		setAttribute("type", "targets");
		if (verbosity >= 100)
			super.target_decl_list__begin();
	}

	public void volatile_stmt(Token label, Token keyword, Token eos) {
		if (!context.getTagName().equals("declaration"))
			contextOpen("declaration");
		setAttribute("type", "volatile");
		super.volatile_stmt(label, keyword, eos);
	}

	public void implicit_stmt(Token label, Token implicitKeyword, Token noneKeyword, Token eos,
			boolean hasImplicitSpecList) {
		if (!context.getTagName().equals("declaration"))
			contextOpen("declaration");
		if (verbosity >= 20)
			super.implicit_stmt(label, implicitKeyword, noneKeyword, eos, hasImplicitSpecList);
		setAttribute("type", "implicit");
		setAttribute("subtype", noneKeyword == null ? "some" : "none");
		contextClose("declaration");
		contextOpen("declaration");
	}

	public void namelist_stmt(Token label, Token keyword, Token eos, int count) {
		contextCloseAllInner("namelists");
		super.namelist_stmt(label, keyword, eos, count);
		setAttribute("count", count);
	}

	public void namelist_group_name(Token id) {
		if (!context.getTagName().equals("declaration"))
			contextOpen("declaration");
		setAttribute("type", "namelists");
		contextOpen("namelists");
		contextOpen("names");
		if (verbosity >= 100)
			super.namelist_group_name(id);
		setAttribute("id", id);
	}

	public void namelist_group_object_list(int count) {
		contextCloseAllInner("names");
		setAttribute("count", count);
		if (verbosity >= 100)
			super.namelist_group_object_list(count);
		contextClose("names");
	}

	public void equivalence_set_list__begin() {
		if (!context.getTagName().equals("declaration"))
			contextOpen("declaration");
		setAttribute("type", "equivalence");
		contextOpen("equivalents");
		if (verbosity >= 100)
			super.equivalence_set_list__begin();
		contextOpen("equivalent");
	}

	public void equivalence_set_list(int count) {
		contextClose("equivalent");
		contextCloseAllInner("equivalents");
		setAttribute("count", count);
		if (verbosity >= 100)
			super.equivalence_set_list(count);
		contextClose("equivalents");
	}

	public void equivalence_object() {
		contextClose("equivalent");
		if (verbosity >= 100)
			super.equivalence_object();
		contextOpen("equivalent");
	}

	public void equivalence_object_list__begin() {
		// TODO Auto-generated method stub
		super.equivalence_object_list__begin();
	}

	public void equivalence_object_list(int count) {
		// TODO Auto-generated method stub
		super.equivalence_object_list(count);
	}

	public void common_block_name(Token id) {
		if (!context.getTagName().equals("declaration"))
			contextOpen("declaration");
		setAttribute("type", "common");
		super.common_block_name(id);
	}

	public void common_block_object_list__begin() {
		contextOpen("objects");
		if (verbosity >= 100)
			super.common_block_object_list__begin();
	}

	public void common_block_object_list(int count) {
		contextCloseAllInner("objects");
		if (verbosity >= 100)
			super.common_block_object_list(count);
		setAttribute("count", count);
		contextClose("objects");
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

	public void substring_range(boolean hasLowerBound, boolean hasUpperBound) {
		Element outerContext = context;
		Element lowerBound = null;
		Element upperBound = null;
		if (hasLowerBound)
			lowerBound = contextNode(-1);
		if (hasUpperBound) {
			upperBound = lowerBound;
			if (hasLowerBound)
				lowerBound = contextNode(-2);
			else
				lowerBound = null;
		}
		contextOpen("name");
		contextOpen("range");
		if (lowerBound != null) {
			contextOpen("lower-bound");
			outerContext.removeChild(lowerBound);
			context.appendChild(lowerBound);
			contextClose();
		}
		if (upperBound != null) {
			contextOpen("upper-bound");
			outerContext.removeChild(upperBound);
			context.appendChild(upperBound);
			contextClose();
		}
		if (verbosity >= 100)
			super.substring_range(hasLowerBound, hasUpperBound);
		contextClose("range");
	}

	public void part_ref(Token id, boolean hasSectionSubscriptList, boolean hasImageSelector) {
		Element outer_context = context;
		Element e = null;
		if (hasSectionSubscriptList) {
			e = contextNode(-1);
			if (e.getTagName() != "subscripts") {
				System.err.println("tag name is not 'subscripts' but '" + e.getTagName() + "'");
				cleanUpAfterError();
			}
		}
		contextOpen("name");
		setAttribute("id", id);
		setAttribute("hasSubscripts", hasSectionSubscriptList);
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
		context = contextNode(-1); // temporarily reopen previously-closed context
		if (verbosity >= 100)
			super.primary();
		contextClose(); // re-close previously closed context
		if (context.getTagName().equals("index-variable")) {
			Element indexVariableContext = context;
			ArrayList<Element> indexVariableNodes = contextNodes();
			boolean hasLowerBound = false;
			boolean hasUpperBound = false;
			boolean hasStep = false;
			ArrayList<Element> unassignedNodes = new ArrayList<Element>();
			for (Element node : indexVariableNodes) {
				if (node.getTagName().equals("lower-bound"))
					hasLowerBound = true;
				else if (node.getTagName().equals("upper-bound"))
					hasUpperBound = true;
				else if (node.getTagName().equals("step"))
					hasStep = true;
				else
					unassignedNodes.add(node);
			}
			if (unassignedNodes.size() > 0) {
				if (!hasLowerBound)
					contextOpen("lower-bound");
				else if (!hasUpperBound)
					contextOpen("upper-bound");
				else if (!hasStep)
					contextOpen("step");
				for (Element node : unassignedNodes) {
					indexVariableContext.removeChild(node);
					context.appendChild(node);
				}
				contextClose();
			}
		}
	}

	public void parenthesized_expr() {
		context = contextNode(-1); // temporarily reopen previously-closed context
		if (verbosity >= 100)
			super.parenthesized_expr();
		contextClose(); // re-close previously closed context
	}

	public void power_operand(boolean hasPowerOperand) {
		if (verbosity >= 100)
			super.power_operand(hasPowerOperand);
		if (!hasPowerOperand)
			return;
		// contextClose("operand");
		contextClose("operation");
	}

	public void power_operand__power_op(Token powerOp) {
		contextClose("operand");
		if (verbosity >= 100)
			super.power_operand__power_op(powerOp);
		if (!context.getTagName().equals("operation")) {
			System.err.println("current context is not 'operation' but '" + context.getTagName() + "'");
			cleanUpAfterError();
		}
		setAttribute("operator", powerOp);
	}

	public void mult_operand(int numMultOps) {
		if (verbosity >= 100)
			super.mult_operand(numMultOps);
		if (numMultOps == 0)
			return;
		// contextClose("operand");
		contextClose("operation");
	}

	public void mult_operand__mult_op(Token multOp) {
		contextClose("operand");
		if (verbosity >= 100)
			super.mult_operand__mult_op(multOp);
		if (!context.getTagName().equals("operation")) {
			System.err.println("current context is not 'operation' but '" + context.getTagName() + "'");
			cleanUpAfterError();
		}
		if (multOp == null)
			throw new NullPointerException();
		if (context.hasAttribute("operator") && !context.getAttribute("operator").equals(multOp.getText()))
			throw new RuntimeException("blah blah" + context.getAttribute("operator"));
		setAttribute("operator", multOp);
	}

	public void signed_operand(Token addOp) {
		Element operationContext = null;
		ArrayList<Element> operands = null;
		if (addOp != null) {
			if (!context.getTagName().equals("operand")) {
				System.err.println("current context is not 'operand' but '" + context.getTagName() + "'");
				cleanUpAfterError();
			}
			// ArrayList<Element> nodes = contextNodes();
			// Element operand = nodes.get(nodes.size() - 1);
			Element operandContext = context;
			// contextOpen("operation");
			setAttribute("type", "unary");
			setAttribute("operator", addOp);
			// contextOpen("operand");
			// outerContext.removeChild(operand);
			// context.appendChild(operand);
			contextClose("operand");
			operationContext = context;
			ArrayList<Element> operationNodes = contextNodes();
			operands = new ArrayList<Element>();
			for (Element node : operationNodes)
				if (node != operandContext && node.getTagName().equals("operand"))
					operands.add(node);
			if (operands.size() != 1) {
				System.err.println("exactly 1 operand expected");
				cleanUpAfterError();
			}
		}
		if (verbosity >= 100)
			super.signed_operand(addOp);
		if (addOp != null) {
			contextClose("operation");
			for (Element operand : operands) {
				operationContext.removeChild(operand);
				for (Element operandExpr : contextNodes(operand))
					context.insertBefore(operandExpr, operationContext);
			}
		}
	}

	public void add_operand(int numAddOps) {
		if (verbosity >= 100)
			super.add_operand(numAddOps);
		if (numAddOps > 0)
			contextClose("operation");
	}

	public void add_operand__add_op(Token addOp) {
		contextClose("operand");
		if (verbosity >= 100)
			super.add_operand__add_op(addOp);
		setAttribute("operator", addOp, "operation");
	}

	public void level_2_expr(int numConcatOps) {
		if (numConcatOps > 0)
			contextClose("operand");
		if (verbosity >= 100)
			super.level_2_expr(numConcatOps);
		if (numConcatOps > 0)
			contextClose("operation");
	}

	public void power_op(Token powerKeyword) {
		Element previousContext = contextNode(-1);
		Element outerContext = context;
		contextOpen("operation");
		setAttribute("type", "binary");
		setAttribute("operator", powerKeyword);
		contextOpen("operand");
		outerContext.removeChild(previousContext);
		context.appendChild(previousContext);
		contextClose("operand");
		if (verbosity >= 100)
			super.power_op(powerKeyword);
		contextOpen("operand");
	}

	public void mult_op(Token multKeyword) {
		Element previousContext = contextNode(-1);
		Element outerContext = context;
		contextOpen("operation");
		setAttribute("type", "binary");
		contextOpen("operand");
		outerContext.removeChild(previousContext);
		context.appendChild(previousContext);
		contextClose("operand");
		if (verbosity >= 100)
			super.mult_op(multKeyword);
		contextOpen("operand");
	}

	public void add_op(Token addKeyword) {
		if (context.getTagName() == "operation") {
			// TODO
		} else {
			Element previousContext = contextNode(-1);
			Element outerContext = context;
			contextOpen("operation");
			setAttribute("type", "binary");
			contextOpen("operand");
			outerContext.removeChild(previousContext);
			context.appendChild(previousContext);
		}
		if (context.getTagName().equals("operand"))
			contextClose("operand");
		if (verbosity >= 100)
			super.add_op(addKeyword);
		contextOpen("operand");
	}

	public void level_3_expr(Token relOp) {
		if (relOp != null)
			contextClose("operand");
		if (verbosity >= 80)
			super.level_3_expr(relOp);
		if (relOp != null) {
			setAttribute("operator", relOp);
			contextClose("operation");
		}
	}

	public void concat_op(Token concatKeyword) {
		if (context.getTagName().equals("operand"))
			contextClose("operand");
		if (context.getTagName() == "operation") {
			// TODO
		} else {
			Element previousContext = contextNode(-1);
			Element outerContext = context;
			contextOpen("operation");
			setAttribute("type", "binary");
			contextOpen("operand");
			outerContext.removeChild(previousContext);
			context.appendChild(previousContext);
			contextClose("operand");
		}
		if (verbosity >= 100)
			super.concat_op(concatKeyword);
		setAttribute("operator", "//");
		contextOpen("operand");
	}

	public void rel_op(Token relOp) {
		Element previousContext = contextNode(-1);
		Element outerContext = context;
		contextOpen("operation");
		setAttribute("type", "binary");
		contextOpen("operand");
		outerContext.removeChild(previousContext);
		context.appendChild(previousContext);
		contextClose("operand");
		if (verbosity >= 100)
			super.rel_op(relOp);
		contextOpen("operand");
	}

	public void assignment_stmt(Token label, Token eos) {
		/*
		if (nodes.size() < 2) {
			System.err.println("there should be at least 2 nodes for 'assignment' but " + nodes.size() + " found");
			cleanUpAfterError();
		}
		*/
		Element target = contextNode(-2);
		Element value = contextNode(-1);
		Element outerContext = context;
		contextOpen("assignment");
		contextOpen("target");
		outerContext.removeChild(target);
		context.appendChild(target);
		contextClose("target");
		contextOpen("value");
		outerContext.removeChild(value);
		context.appendChild(value);
		contextClose("value");
		if (verbosity >= 100)
			super.assignment_stmt(label, eos);
		contextClose("assignment");
	}

	public void forall_header() {
		contextClose("header");
		if (verbosity >= 100)
			super.forall_header();
		contextOpen("body");
		contextOpen("statement");
	}

	public void forall_triplet_spec(Token id, boolean hasStride) {
		contextCloseAllInner("index-variable");
		setAttribute("name", id);
		super.forall_triplet_spec(id, hasStride);
		contextClose("index-variable");
		contextOpen("index-variable");
	}

	public void forall_triplet_spec_list__begin() {
		if (contextTryFind("loop") == null) {
			contextRename("statement", "loop");
			setAttribute("type", "forall");
			setAttribute("subtype", "concurrent");
			contextOpen("header");
		}
		contextOpen("index-variables");
		if (verbosity >= 100)
			super.forall_triplet_spec_list__begin();
		contextOpen("index-variable");
	}

	public void forall_triplet_spec_list(int count) {
		contextClose("index-variable");
		setAttribute("count", count);
		if (verbosity >= 100)
			super.forall_triplet_spec_list(count);
		contextClose("index-variables");
	}

	public void forall_stmt__begin() {
		contextRename("statement", "loop");
		setAttribute("type", "forall");
		if (verbosity >= 100)
			super.forall_stmt__begin();
		contextOpen("header");
	}

	public void forall_stmt(Token label, Token forallKeyword) {
		contextCloseAllInner("loop");
		super.forall_stmt(label, forallKeyword);
		contextClose("loop");
		contextOpen("statement"); // TODO: temporary workaround
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
		ArrayList<Element> nodes = contextNodes();
		Element outer_context = context;
		contextOpen("header");
		for (Element node : nodes) {
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
		Element condition = contextNode(-1);
		Element outerContext = context;
		contextClose("body");
		// contextOpen("else");
		// contextOpen("if");
		contextOpen("header");
		setAttribute("type", "else-if");
		outerContext.removeChild(condition);
		context.appendChild(condition);
		contextClose("header");
		if (verbosity >= 80)
			super.else_if_stmt(label, elseKeyword, ifKeyword, thenKeyword, id, eos);
		contextOpen("body");
		setAttribute("type", "else-if");
		contextOpen("statement");
	}

	public void else_stmt(Token label, Token elseKeyword, Token id, Token eos) {
		contextClose("body");
		if (verbosity >= 80)
			super.else_stmt(label, elseKeyword, id, eos);
		contextOpen("body");
		setAttribute("type", "else");
		contextOpen("statement");
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
		if (context.getTagName() != "header") {
			contextOpen("loop");
			contextOpen("header");
		}
		contextClose("header");
		if (digitString != null) {
			setAttribute("label", digitString);
		}
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
		if (!context.getTagName().equals("statement"))
			contextClose("index-variable");
		super.loop_control(whileKeyword, doConstructType, hasOptExpr);
		setAttribute("subtype", doConstructType, "loop");
	}

	public void do_variable(Token id) {
		if (context.getTagName().equals("statement")) {
			contextRename("statement", "loop");
			setAttribute("type", "do");
		} else if (context.getTagName().equals("value")) {
			// nothing special needed
			// contextClose();
			// contextRename("array-constructor-values", "loop");
			// setAttribute("type", "array-constructor");
		} else {
			System.err.println("unexpected context of 'do-variable': '" + context.getTagName() + "'");
			cleanUpAfterError();
		}
		contextOpen("header");
		contextOpen("index-variable");
		setAttribute("name", id);
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

	public void continue_stmt(Token label, Token continueKeyword, Token eos) {
		contextOpen("statement");
		super.continue_stmt(label, continueKeyword, eos);
	}

	public void input_item() {
		contextClose("input");
		if (verbosity >= 100)
			super.input_item();
		contextOpen("input");
	}

	public void input_item_list__begin() {
		contextOpen("inputs");
		if (verbosity >= 100)
			super.input_item_list__begin();
		contextOpen("input");
	}

	public void input_item_list(int count) {
		contextClose("input");
		if (verbosity >= 100)
			super.input_item_list(count);
		setAttribute("count", count);
		contextClose("inputs");
	}

	public void output_item() {
		contextClose("output");
		if (verbosity >= 100)
			super.output_item();
		contextOpen("output");
	}

	public void output_item_list__begin() {
		contextOpen("outputs");
		if (verbosity >= 100)
			super.output_item_list__begin();
		contextOpen("output");
	}

	public void output_item_list(int count) {
		contextClose("output");
		if (verbosity >= 100)
			super.output_item_list(count);
		setAttribute("count", count);
		contextClose("outputs");
	}

	public void io_implied_do_object() {
		if (verbosity >= 100)
			super.io_implied_do_object();
		contextOpen("statement");
	}

	public void format_item_list__begin() {
		// contextOpen("declaration");
		if (verbosity >= 100)
			super.format_item_list__begin();
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
		contextOpen("declaration");
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
		contextOpen("header");
	}

	public void module_stmt(Token label, Token moduleKeyword, Token id, Token eos) {
		contextClose("header");
		setAttribute("name", id);
		super.module_stmt(label, moduleKeyword, id, eos);
		contextOpen("body");
		contextOpen("specification");
		contextOpen("declaration");
	}

	public void end_module_stmt(Token label, Token endKeyword, Token moduleKeyword, Token id, Token eos) {
		contextClose("body");
		super.end_module_stmt(label, endKeyword, moduleKeyword, id, eos);
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
		contextOpen("declaration");
	}

	public void interface_block() {
		// TODO Auto-generated method stub
		super.interface_block();
	}

	public void interface_specification() {
		// TODO Auto-generated method stub
		super.interface_specification();
	}

	public void interface_stmt__begin() {
		contextOpen("declaration");
		contextOpen("interface");
		if (verbosity >= 100)
			super.interface_stmt__begin();
		contextOpen("header");
	}

	public void interface_stmt(Token label, Token abstractToken, Token keyword, Token eos, boolean hasGenericSpec) {
		Element previous_context = context;
		contextClose("header");
		if (context.getTagName() != "interface") {
			context = previous_context;
			contextOpen("declaration");
			contextOpen("interface");
			contextOpen("header");
			contextClose("header");
		}
		super.interface_stmt(label, abstractToken, keyword, eos, hasGenericSpec);
		if (abstractToken != null) // && abstractToken.getText().toLowerCase() == "abstract")
			setAttribute("type", abstractToken);
		contextOpen("body");
		contextOpen("specification");
		contextOpen("declaration");
	}

	public void end_interface_stmt(Token label, Token kw1, Token kw2, Token eos, boolean hasGenericSpec) {
		contextCloseAllInner("interface");
		super.end_interface_stmt(label, kw1, kw2, eos, hasGenericSpec);
		contextClose("interface");
	}

	public void interface_body(boolean hasPrefix) {
		// TODO Auto-generated method stub
		super.interface_body(hasPrefix);
	}

	public void import_stmt(Token label, Token importKeyword, Token eos, boolean hasGenericNameList) {
		if (context.getTagName() != "declaration")
			contextOpen("declaration");
		setAttribute("type", "import");
		super.import_stmt(label, importKeyword, eos, hasGenericNameList);
		contextClose("declaration");
	}

	public void external_stmt(Token label, Token externalKeyword, Token eos) {
		if (context.getTagName() != "declaration")
			contextOpen("declaration");
		if (verbosity >= 80)
			super.external_stmt(label, externalKeyword, eos);
		setAttribute("type", "external");
	}

	public void procedure_declaration_stmt(Token label, Token procedureKeyword, Token eos, boolean hasProcInterface,
			int count) {
		// TODO Auto-generated method stub
		super.procedure_declaration_stmt(label, procedureKeyword, eos, hasProcInterface, count);
	}

	public void proc_decl(Token id, boolean hasNullInit) {
		contextOpen("procedure");
		setAttribute("name", id);
		if (verbosity >= 80)
			super.proc_decl(id, hasNullInit);
		contextClose("procedure");
	}

	public void proc_decl_list__begin() {
		if (!context.getTagName().equals("declaration"))
			contextOpen("declaration");
		setAttribute("type", "procedures");
		contextOpen("procedures");
		if (verbosity >= 100)
			super.proc_decl_list__begin();
	}

	public void proc_decl_list(int count) {
		contextCloseAllInner("procedures");
		setAttribute("count", count);
		if (verbosity >= 100)
			super.proc_decl_list(count);
		contextClose("procedures");
	}

	public void intrinsic_stmt(Token label, Token intrinsicKeyword, Token eos) {
		Element condition = contextNode(-1);
		Element outerContext = context;
		if (!context.getTagName().equals("declaration"))
			contextOpen("declaration");
		setAttribute("type", "intrinsic");
		outerContext.removeChild(condition);
		context.appendChild(condition);
		super.intrinsic_stmt(label, intrinsicKeyword, eos);
	}

	public void call_stmt(Token label, Token callKeyword, Token eos, boolean hasActualArgSpecList) {
		Element outerContext = context;
		ArrayList<Element> nodes = contextNodes();
		Element name = nodes.get(nodes.size() - 1);
		Element arguments = null;
		if (name.getTagName() == "arguments") {
			arguments = name;
			name = nodes.get(nodes.size() - 2);
		} else if (name.getTagName() != "name") {
			System.err.println("tag name is not 'name' but '" + name.getTagName() + "'");
			cleanUpAfterError();
		}
		contextOpen("call");
		outerContext.removeChild(name);
		context.appendChild(name);
		if (arguments != null) {
			outerContext.removeChild(arguments);
			context.appendChild(arguments);
		}
		super.call_stmt(label, callKeyword, eos, hasActualArgSpecList);
		contextClose("call");
	}

	public void procedure_designator() {
		if (verbosity >= 100)
			super.procedure_designator();
		setAttribute("type", "procedure");
		contextClose("name");
	}

	public void actual_arg_spec(Token keyword) {
		if (verbosity >= 100)
			super.actual_arg_spec(keyword);
		contextOpen("argument");
	}

	public void actual_arg_spec_list__begin() {
		contextOpen("arguments");
		if (verbosity >= 100)
			super.actual_arg_spec_list__begin();
		contextOpen("argument");
	}

	public void actual_arg_spec_list(int count) {
		contextClose("argument");
		setAttribute("count", count);
		if (verbosity >= 100)
			super.actual_arg_spec_list(count);
		contextClose("arguments");
	}

	public void actual_arg(boolean hasExpr, Token label) {
		if (contextTryFind("argument") != null)
			contextClose("argument");
		if (verbosity >= 60)
			super.actual_arg(hasExpr, label);
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
		contextOpen("declaration");
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
		contextOpen("declaration");
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
		if (contextTryFind("file") != null) {
			if (context.getTagName().equals("declaration"))
				contextClose("declaration");
			contextOpen("declaration");
			setAttribute("type", "include");
		}
		contextOpen("file");
		if (verbosity >= 100)
			super.start_of_file(filename, path);
		if (path.equals("ERROR_FILE_NOT_FOUND"))
			setAttribute("path", filename);
		else
			setAttribute("path", path);
	}

	public void end_of_file(String filename, String path) {
		contextCloseAllInner("file");
		if (verbosity >= 100)
			super.end_of_file(filename, path);
		contextClose("file");
	}

	public void cleanUpAfterError() {
		new RuntimeException("Aborting construction of the AST.").printStackTrace();
		cleanUp();
		System.exit(1);
	}

	public void cleanUp() {
		while (context != root)
			contextClose(context);
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
