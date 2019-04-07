package fortran.ofp;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.antlr.runtime.Token;
import org.apache.commons.cli.CommandLine;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;

import fortran.ofp.parser.java.TokensList;
import fortran.ofp.parser.java.CodeBounds;
import fortran.ofp.parser.java.FortranLexer;
import fortran.ofp.parser.java.FortranParserActionPrint;
import fortran.ofp.parser.java.IFortranParser;

/**
 * Base class for XML output generator for Open Fortran Parser.
 *
 * @author Mateusz Bysiek https://mbdevpl.github.io/
 */
public class XMLPrinterBase extends FortranParserActionPrint {

	private static final Logger LOG = Logger.getLogger(XMLPrinterBase.class.getName());

	/**
	 * Parsed command-line arguments.
	 */
	private CommandLine cmd;

	/**
	 * Verbosity level from 0 to 100.
	 */
	protected int verbosity;

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

	static public ArrayList<String> tokenLocationsWhitelist = new ArrayList<String>(
			Arrays.asList(new String[] { "file", "members", "body", "specification" }));

	static public Map<String, String> listContexts = new HashMap<String, String>();

	static public void addListContext(String eventNamePrefix, String listName, String elementName) {
		listContexts.put(eventNamePrefix + "-list__begin", listName);
		listContexts.put(eventNamePrefix + "-list-part", elementName);
		listContexts.put(eventNamePrefix + "-list", listName);
	}

	static {
		addListContext("generic-name", "names", "name");
		addListContext("label", "labels", "label");
		/*
		addListContext("type-attr-spec", "", "");
		addListContext("type-param-decl", "", "");
		addListContext("component-attr-spec", "", "");
		addListContext("component-decl", "", "");
		addListContext("deferred-shape-spec", "", "");
		addListContext("proc-component-attr-spec", "", "");
		addListContext("binding-attr", "", "");
		addListContext("type-param-spec", "", "");
		addListContext("component-spec", "", "");
		addListContext("enumerator", "", "");
		*/
		// addListContext("ac-value", /*"array-constructor-values"*/ null, /*"value"*/ null); // not straightforward
		addListContext("entity-decl", "variables", "variable");
		/*
		addListContext("explicit-shape-spec", "", "");
		*/
		// addListContext("access-id", /*"access-list"*/ null, null); // currently not necessary
		// addListContext("allocatable-decl", null, null); // currently not necessary
		/*
		addListContext("bind-entity", "", "");
		*/
		// addListContext("codimension-decl", null, null); // currently not necessary
		addListContext("data-stmt-object", "variables", null);
		/*
		addListContext("data-i-do-object", "", "");
		*/
		addListContext("data-stmt-value", "values", null);
		addListContext("named-constant-def", "constants", null);
		addListContext("pointer-decl", "names", null);
		/*
		addListContext("cray-pointer-assoc", "", "");
		*/
		addListContext("saved-entity", "names", "name");
		// addListContext("target-decl", null, null); // currently not necessary
		/*
		addListContext("implicit-spec", "", "");
		*/
		addListContext("letter-spec", "letter-ranges", null);
		/*
		addListContext("namelist-group-object", "", "");
		*/
		addListContext("equivalence-set", "equivalents", "equivalent");
		// addListContext("equivalence-object", null, null); // currently not necessary
		addListContext("common-block-object", "objects", null);
		addListContext("section-subscript", "subscripts", /*"subscript"*/ null);
		addListContext("alloc-opt", "keyword-arguments", null);
		/*
		addListContext("cosubscript", "", "");
		*/
		addListContext("allocation", "expressions", null);
		addListContext("allocate-object", "expressions", null);
		/*
		addListContext("allocate-shape-spec", "", "");
		*/
		addListContext("pointer-object", "pointers", "pointer");
		addListContext("dealloc-opt", "keyword-arguments", null);
		/*
		addListContext("allocate-coshape-spec", "", "");
		addListContext("bounds-spec", "", "");
		addListContext("bounds-remapping", "", "");
		*/
		addListContext("forall-triplet-spec", "index-variables", null);
		addListContext("case-value-range", "value-ranges", null);
		addListContext("association", "keyword-arguments", null);
		/*
		addListContext("sync-stat", "", "");
		addListContext("lock-stat", "", "");
		*/
		addListContext("connect-spec", "keyword-arguments", "keyword-argument");
		addListContext("close-spec", "keyword-arguments", "keyword-argument");
		addListContext("io-control-spec", "io-controls", null);
		addListContext("input-item", "inputs", null);
		addListContext("output-item", "outputs", null);
		/*
		addListContext("wait-spec", "", "");
		addListContext("position-spec", "", "");
		addListContext("flush-spec", "", "");
		addListContext("inquire-spec", "", "");
		*/
		addListContext("format-item", "format-items", null);
		/*
		addListContext("v", "", "");
		*/
		addListContext("rename", "rename", null);
		addListContext("only", "only", null);
		addListContext("proc-decl", "procedures", null);
		addListContext("actual-arg-spec", "arguments", "argument");
		addListContext("dummy-arg", "arguments", null);
	}

	public XMLPrinterBase(String[] args, IFortranParser parser, String filename) {
		super(args, parser, filename);

		// System.err.println(Arrays.toString(args));
		cmd = new XmlPrinterArgsParser().parse(args);
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
		Element newContext = doc.createElement(name);
		if (context != null)
			context.appendChild(newContext);
		context = newContext;
		return context;
	}

	protected ArrayList<Element> contextHierarchy(Element context) {
		ArrayList<Element> hierarchy = new ArrayList<Element>();
		hierarchy.add(context);
		Element found = context;
		while (found != root && found.getParentNode() != null) {
			found = (Element) found.getParentNode();
			hierarchy.add(found);
		}
		return hierarchy;
	}

	protected ArrayList<Element> contextHierarchy() {
		return contextHierarchy(context);
	}

	protected ArrayList<String> contextNameHierarchy(Element context) {
		ArrayList<String> names = new ArrayList<String>();
		for (Element found : contextHierarchy(context))
			names.add(found.getTagName());
		return names;
	}

	protected ArrayList<String> contextNameHierarchy() {
		return contextNameHierarchy(context);
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
		for (String name : contextNameHierarchy())
			System.err.println("  " + name);
		cleanUpAfterError();
		return null;
	}

	/**
	 * Rename given context to given name.
	 *
	 * @param context
	 * @param toName
	 */
	protected void contextRename(Element context, String toName) {
		doc.renameNode(context, null, toName);
	}

	protected void contextRename(String toName) {
		contextRename(context, context.getTagName(), toName);
	}

	/**
	 * Rename given context to given name after making sure about its current name.
	 *
	 * @param context
	 * @param fromName
	 * @param toName
	 */
	protected void contextRename(Element context, String fromName, String toName) {
		if (context.getTagName() != fromName)
			cleanUpAfterError("Cannot rename current context from '" + fromName + "' to '" + toName
					+ "' because its name is '" + context.getTagName() + "'.");
		contextRename(context, toName);
	}

	protected void contextRename(String fromName, String toName) {
		contextRename(context, fromName, toName);
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
			cleanUpAfterError("Cannot close given context because it is root node of the document.");
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
	 * Collection of attributes of given XML context.
	 *
	 * @param context the XML context to be queried
	 * @return list of attributes
	 */
	protected ArrayList<Attr> contextAttributes(Element context) {
		NamedNodeMap attributesMap = context.getAttributes();
		ArrayList<Attr> attributes = new ArrayList<Attr>();
		for (int i = 0; i < attributesMap.getLength(); i++)
			attributes.add((Attr) attributesMap.item(i));
		return attributes;
	}

	protected ArrayList<Attr> contextAttributes() {
		return contextAttributes(context);
	}

	/**
	 * Collection of children nodes of given XML context.
	 *
	 * @param context the XML context to be queried
	 * @param begin_index the index will be chosen from the end if negative number is given
	 * @param count number of results to return, return all results if zero is given
	 * @return list of nodes
	 */
	protected ArrayList<Element> contextNodes(Element context, int beginIndex, int count) {
		NodeList nodeList = context.getChildNodes();
		int nodeListLength = nodeList.getLength();
		ArrayList<Element> nodes = new ArrayList<Element>();
		// System.err.println("contextNodes of " + context + " " + beginIndex + " " + count);
		if (count == 0 && nodeListLength == 0)
			return nodes;
		if (beginIndex < 0)
			beginIndex = nodeListLength + beginIndex;
		if (beginIndex < 0 || beginIndex >= nodeListLength)
			// throw new IndexOutOfBoundsException(
			cleanUpAfterError("starting index " + beginIndex + " out of bounds [" + 0 + ", " + nodeListLength + ")");
		if (count == 0)
			count = nodeListLength - beginIndex;
		if (count < 0)
			// throw new IndexOutOfBoundsException(
			cleanUpAfterError("attemted to return " + count + " number of nodes");
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

	protected int contextNodesCount(Element context) {
		return context.getChildNodes().getLength();
	}

	protected int contextNodesCount() {
		return contextNodesCount(context);
	}

	protected Element contextNode(Element context, int index) {
		return contextNodes(context, index, 1).get(0);
	}

	protected Element contextNode(int index) {
		return contextNode(context, index);
	}

	protected String contextString(Element context) {
		if (context == null)
			return "context is null";
		ArrayList<String> names = new ArrayList<String>();
		for (Element node : contextNodes(context))
			names.add(node.getTagName());
		return "context: " + context.getTagName() + "\n" + "  attributes: " + contextAttributes(context) + "\n"
				+ "  sub-contexts: " + names;
	}

	protected void contextPrint(Element context) {
		// System.err.println(contextString(context));
		if (context == null) {
			System.err.println("context is null");
			return;
		}
		System.err.println("context: " + context.getTagName());
		System.err.println("  attributes: " + contextAttributes(context));
		ArrayList<String> names = new ArrayList<String>();
		for (Element node : contextNodes(context))
			names.add(node.getTagName());
		System.err.println("  sub-contexts: " + names);
	}

	protected Attr getAttribute(String name, Element context) {
		return (Attr) context.getAttributes().getNamedItem(name);
	}

	protected Attr getAttribute(String name) {
		return getAttribute(name, context);
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
		else if (value instanceof Token) {
			Token token = (Token) value;
			valueString = token.getText();
			if (verbosity >= 100) {
				CodeBounds bounds = new CodeBounds(context);
				bounds.extend(token);
				bounds.persist(context);
			}
		} else
			valueString = value.toString();
		context.setAttribute(name, valueString);
	}

	protected void setAttribute(String name, Object value, String... names) {
		setAttribute(name, value, contextFind(names));
	}

	protected void setAttribute(String name, Object value) {
		setAttribute(name, value, context);
	}

	/**
	 * Return null if (line, col) not in this context, and when it cannot be determined if it is in it or not.
	 *
	 * Otherwise, return an innermost context which contains a given location.
	 */
	public Element findContext(Element context, int line, int col) {
		for (Element node : contextNodes(context)) {
			Element containingNode = findContext(node, line, col);
			if (containingNode == null)
				continue;
			return containingNode;
		}
		CodeBounds bounds = new CodeBounds(context);
		if (bounds.begin == null || bounds.end == null)
			return null;
		if (line < bounds.begin.line || line > bounds.end.line)
			return null;
		if (line > bounds.begin.line && line < bounds.end.line)
			return context;
		if (line == bounds.begin.line)
			return col >= bounds.begin.col ? context : null;
		if (line == bounds.end.line)
			return col <= bounds.end.col ? context : null;
		throw new RuntimeException();
	}

	public int findPosition(Element context, int line, int col) {
		int index = -1;
		for (Element node : contextNodes(context)) {
			CodeBounds bounds = new CodeBounds(node);
			++index;
			if (bounds.begin == null || bounds.end == null)
				continue;
			if (line < bounds.begin.line)
				return index;
			if (line > bounds.end.line)
				continue;
			if (line == bounds.begin.line)
				if (col < bounds.begin.col)
					return index;
			if (col > bounds.end.col)
				continue;
			throw new RuntimeException("looking for (" + line + "," + col + ")" + " within bounds " + bounds + "\n"
					+ "of " + contextString(node) + "\n" + "subnode of " + contextString(context));
		}
		return contextNodesCount(context);
	}

	/**
	 * Propagate code bounds within a given context.
	 *
	 * Propagating code bounds means that code bounds of each node within given context are extended using bounds of all
	 * its subnodes. This is done recursively, depth-first.
	 *
	 * An exception to this rule are <file> nodes, the bounds of which are not propagated outside of them.
	 */
	protected void propagateBounds(Element context) {
		ArrayList<Element> nodes = contextNodes(context);
		for (Element node : nodes) {
			propagateBounds(node);
			if (context == root)
				continue;
			if (node.getNodeName().equals("file"))
				continue; // propagating bounds beyond <file> node makes them inconsistent
			CodeBounds bounds = new CodeBounds(node);
			if (bounds.begin == null)
				continue;
			CodeBounds rootBounds = new CodeBounds(context);
			rootBounds.extend(bounds.begin);
			rootBounds.extend(bounds.end);
			rootBounds.persist(context);
		}
	}

	/**
	 * Move given element from its current context to a given target context.
	 *
	 * The element is inserted at target index - so that later contextNode(targetContext, targetIndex) will return the
	 * element.
	 *
	 * The index, as in contextNode() method, can be negative.
	 *
	 * Unlike contextNode() method the index can be also null, which appends the element at the end of target context.
	 *
	 * @param targetContext
	 * @param targetIndex
	 * @param element
	 */
	protected void moveTo(Element targetContext, Integer targetIndex, Element element) {
		if (targetContext == element)
			cleanUpAfterError("Cannot move " + element + " to itself.");
		try {
			element.getParentNode().removeChild(element);
			boolean insert = false;
			if (targetIndex != null)
				if (targetIndex < 0) {
					targetIndex += 1;
					if (targetIndex < 0)
						insert = true;
				} else
					insert = true;
			if (insert)
				targetContext.insertBefore(element, contextNode(targetContext, targetIndex));
			else
				targetContext.appendChild(element);
		} catch (org.w3c.dom.DOMException error) {
			System.err.println("Cannot move " + element + " to " + targetContext + ".");
			contextPrint(element);
			System.err.println(contextNameHierarchy(element));
			contextPrint(targetContext);
			System.err.println(contextNameHierarchy(targetContext));
			cleanUpAfterError(error);
		}
	}

	protected void moveTo(Element targetContext, Element element) {
		moveTo(targetContext, null, element);
	}

	protected void moveHere(Integer targetIndex, Element element) {
		moveTo(context, targetIndex, element);
	}

	protected void moveHere(Element element) {
		moveTo(context, null, element);
	}

	protected void moveTo(Element targetContext, Integer targetIndex, ArrayList<Element> elements) {
		for (Element element : elements)
			moveTo(targetContext, targetIndex, element);
	}

	protected void moveTo(Element targetContext, ArrayList<Element> elements) {
		moveTo(targetContext, null, elements);
	}

	protected void moveTo(Integer targetIndex, ArrayList<Element> elements) {
		moveTo(context, targetIndex, elements);
	}

	protected void moveHere(ArrayList<Element> elements) {
		moveTo(context, null, elements);
	}

	protected void printRuleHeader(int rule, String name, String addendum) {
		if (addendum == "list-begin") {
			if (!listContexts.containsKey(name))
				LOG.info("list context not recognized: " + name);
			else
				contextOpen(listContexts.get(name));
		}
		contextOpen(name);
		setAttribute("rule", rule);
		if (addendum.length() > 0)
			setAttribute("addendum", addendum);
	}

	protected void printRuleTrailer() {
		Element innerContext = context;
		Attr addendum = getAttribute("addendum");
		contextClose();
		if (addendum != null) {
			if (addendum.getValue() == "list") {
				String name = innerContext.getTagName();
				if (listContexts.containsKey(name)) {
					contextCloseAllInner(listContexts.get(name));
					setAttribute("count", getAttribute("count", innerContext).getValue());
					moveHere(innerContext);
					contextClose();
				}
			}
			if (addendum.getValue() == "list-begin" || addendum.getValue() == "list") {
				// LOG.log(Level.FINE, "removing {0} from {1}", new Object[]{innerContext, context});
				if (verbosity < 100)
					innerContext.getParentNode().removeChild(innerContext);
			}
		}
	}

	protected void printParameter(Object param, String name) {
		setAttribute(name, param);
	}

	protected void printParameter(Token param, String name) {
		setAttribute(name, param);
	}

	protected void printTokens(Token... tokens) {
		for (Token token : tokens) {
			if (token == null) {
				System.err.println("token is null");
				continue;
			}
			int line = token.getLine();
			int colBegin = token.getCharPositionInLine();
			String text = token.getText();
			int colEnd = colBegin + text.length();
			System.err.println(filename + "@" + line + ":" + colBegin + "~" + colEnd + ": \"" + text + "\"");
		}
		/*
		try {
			TokensList tokens = new TokensList(new File(filename), false);
			System.err.println("found tokens: " + tokens);
		} catch (IOException e) {
		}
		*/
	}

	/**
	 * Insert raw tokens from current file into given context.
	 */
	protected void insertTokens(Element context, int tokenType, String tokenContextName, String tokenTextAttributeName)
			throws IOException {
		// System.err.println("all tokens: " + new TokensList(new File(filename)));
		TokensList tokens = new TokensList(new File(filename), tokenType);
		// System.err.println("found tokens: " + tokens);
		insertTokens(context, tokens, tokenContextName, tokenTextAttributeName);
	}

	protected void insertTokens(Element context, ArrayList<Token> tokens, String tokenContextName,
			String tokenTextAttributeName) {
		for (Token token : tokens)
			insertToken(context, token, tokenContextName, tokenTextAttributeName);
	}

	protected void insertToken(Element context, Token token, String tokenContextName, String tokenTextAttributeName) {
		TokenTarget target = findTarget(context, token);

		Element tokenNode = contextOpen(tokenContextName);
		setAttribute(tokenTextAttributeName, token.getText());
		CodeBounds bounds = new CodeBounds(token);
		bounds.persist(tokenNode); // updateBounds(token);
		contextClose();

		tokenNode.getParentNode().removeChild(tokenNode);
		if (target.index < contextNodesCount(target.element))
			target.element.insertBefore(tokenNode, contextNode(target.element, target.index));
		else if (target.index == contextNodesCount(target.element))
			target.element.appendChild(tokenNode);
		else
			throw new IllegalArgumentException("location within target is invalid");

		propagateBounds(target.element);
	}

	private class TokenTarget {

		public Element element;
		public int index;

		public TokenTarget(Element target, int targetIndex) {
			element = target;
			index = targetIndex;
		}

	}

	private TokenTarget findTarget(Element context, Token token) {
		int line = token.getLine();
		int col_begin = token.getCharPositionInLine();
		Element target = findContext(context, line, col_begin);
		/* debug-only
		int col_end = col_begin + comment.getText().length();
		Element targetAlt = findContext(context, line, col_end);
		*/
		if (target == null /*&& targetAlt == null*/) {
			target = contextNode(root, 0);
			// System.err.println("either in the beginning or at the end...");
			/* debug-only
			} else if (target != targetAlt) {
				contextPrint(target);
				contextPrint(targetAlt);
				throw new IllegalArgumentException();
			*/
		}
		int targetIndex = findPosition(target, line, col_begin);
		/* debug-only
		int targetIndexAlt = findPosition(target, line, col_end);
		if (targetIndex != targetIndexAlt) {
			System.err.println("should be at index " + targetIndex + " or " + targetIndexAlt);
			throw new IllegalArgumentException("two possible targets");
		}
		System.err.println("adjusting " + target.getNodeName() + "@" + targetIndex + "/" + contextNodesCount(target));
		*/
		return refineTarget(token, target, targetIndex);
	}

	private TokenTarget refineTarget(Token token, Element target, int targetIndex) {
		if (contextNodesCount(target) == 0) {
			/*
			System.err.println("target is empty");
			*/
			ArrayList<Element> hierarchy = contextHierarchy(target);
			hierarchy.remove(0);
			for (Element parent : hierarchy) {
				ArrayList<Element> parentNodes = contextNodes(parent);
				int indexInParent = parentNodes.indexOf(target);
				target = parent;
				targetIndex = indexInParent + 1;
				if (XMLPrinterBase.tokenLocationsWhitelist.contains(target.getNodeName()))
					break;
			}
			if (!XMLPrinterBase.tokenLocationsWhitelist.contains(target.getNodeName()))
				throw new IllegalArgumentException(
						"didn't find good candidate to adjust token " + token + " location in hierarchy " + hierarchy);
		}
		boolean updated = false;
		if (targetIndex > 0) {
			Element beforeTarget = contextNode(target, targetIndex - 1);
			if (beforeTarget.getNodeName().equals("body")) {
				target = beforeTarget;
				targetIndex = contextNodesCount(beforeTarget);
				updated = true;
				/*
				System.err.println("beforeTarget: " + target.getNodeName() + "@" + targetIndex + "/" + contextNodesCount(target));
				*/
			}
			/*
			else
				System.err.println("before is " + beforeTarget.getNodeName());
			*/
		}
		if (!updated && targetIndex < contextNodesCount(target) - 1) {
			Element afterTarget = contextNode(target, targetIndex);
			if (afterTarget.getNodeName().equals("body")) {
				target = afterTarget;
				targetIndex = 0;
				updated = true;
				/*
				System.err.println("afterTarget: " + target.getNodeName() + "@" + targetIndex + "/" + contextNodesCount(target));
				*/
			}
			/*
			else
				System.err.println("after is " + afterTarget.getNodeName());
			*/
		}
		return new TokenTarget(target, targetIndex);
	}

	public void persist() throws TransformerException {
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
	}

	public void cleanUpAfterError(String comment, Exception error) {
		if (comment != null)
			System.err.println(comment);
		new RuntimeException("Aborting construction of the AST.", error).printStackTrace();
		cleanUp();
		System.exit(1);
	}

	public void cleanUpAfterError(String comment) {
		cleanUpAfterError(comment, null);
	}

	public void cleanUpAfterError(Exception error) {
		cleanUpAfterError(null, error);
	}

	public void cleanUpAfterError() {
		cleanUpAfterError(null, null);
	}

	public void cleanUp() {
		while (context != root)
			contextClose(context);
		if (verbosity >= 100) {
			propagateBounds(context);
			try {
				insertTokens(context, FortranLexer.LINE_COMMENT, "comment", "text");
				insertTokens(context, FortranLexer.PREPROCESS_LINE, "directive", "text");
			} catch (IOException error) {
				error.printStackTrace();
				System.exit(1);
			}
			propagateBounds(context);
		}
		try {
			persist();
		} catch (Exception error) {
			error.printStackTrace();
			System.exit(1);
		}
	}

}
