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
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;

import fortran.ofp.parser.java.FortranParserActionPrint;
import fortran.ofp.parser.java.IFortranParser;

/**
 * Base class for XML output generator for Open Fortran Parser.
 *
 * @author Mateusz Bysiek https://mbdevpl.github.io/
 */
public class XMLPrinterBase extends FortranParserActionPrint {

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

	public XMLPrinterBase(String[] args, IFortranParser parser, String filename) {
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

	protected void contextPrint(Element context) {
		System.err.println(context);
		System.err.println(contextAttributes(context));
		System.err.println(contextNodes(context));
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

	protected void moveTo(Element targetContext, Element element) {
		if (targetContext == element)
			cleanUpAfterError("Cannot move " + element + " to itself.");
		try {
			element.getParentNode().removeChild(element);
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

	protected void moveTo(Element targetContext, ArrayList<Element> elements) {
		for (Element element : elements)
			moveTo(targetContext, element);
	}

	protected void moveHere(Element element) {
		moveTo(context, element);
	}

	protected void moveHere(ArrayList<Element> elements) {
		moveTo(context, elements);
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

}
