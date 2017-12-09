package fortran.ofp.parser.java;

import static org.junit.jupiter.api.Assertions.*;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.antlr.runtime.Token;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

class CodeBoundsTests {

	Document doc;

	@BeforeEach
	void setUp() throws Exception {
		DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
		DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
		doc = docBuilder.newDocument();
	}

	@Test
	void testExtendPersist2() {
		Element context = doc.createElement("test-node"); // contextOpen("test-node");
		System.err.println("testing end subroutine tokens");
		// printTokens(label, keyword1, keyword2, name, eos);
		CodeBounds bounds;
		bounds = new CodeBounds(598, 0, 598, 3); // bounds = new CodeBounds(keyword1);
		bounds.persist(context);
		bounds = new CodeBounds(context);

		assertEquals((int)bounds.begin.line, 598);
		assertEquals((int)bounds.begin.col, 0);
		assertEquals((int)bounds.end.line, 598);
		assertEquals((int)bounds.end.col, 3);

		bounds.extend(new CodeLocation(598, 4));  //
		assertEquals((int)bounds.end.col, 4);     //		
		bounds.extend(new CodeLocation(598, 14)); // bounds.extend(name);
		bounds.persist(context);
		System.err.println(new CodeBounds(context));
		// bounds.extend(eos);
		bounds.persist(context);
		System.err.println(new CodeBounds(context));

		bounds = new CodeBounds(context);
		assertEquals((int)bounds.begin.line, 598);
		assertEquals((int)bounds.begin.col, 0);
		assertEquals((int)bounds.end.line, 598);
		assertEquals((int)bounds.end.col, 14);
	}

	@Test
	void testExtendPersist() {
		CodeBounds bounds;
		Element e = doc.createElement("blah");
		bounds = new CodeBounds(e);
		bounds.extend(new CodeLocation(10, 0));
		bounds.persist(e);
		bounds = new CodeBounds(e);
		bounds.extend(new CodeLocation(10, 20));
		bounds.persist(e);

		bounds = new CodeBounds(e);
		assertEquals((int)bounds.begin.line, 10);
		assertEquals((int)bounds.end.col, 20);
	}

	@Test
	void testExtend() {
		CodeBounds bounds = new CodeBounds();
		bounds.extend(new CodeLocation(10, 0));
		bounds.extend(new CodeLocation(10, 3));
		bounds.extend(new CodeLocation(10, 10));
		bounds.extend(new CodeLocation(10, 15));
		bounds.extend(new CodeLocation(10, 20));

		assertEquals((int)bounds.begin.line, 10);
		assertEquals((int)bounds.begin.col, 0);
		assertEquals((int)bounds.end.line, 10);
		assertEquals((int)bounds.end.col, 20);
	}

}
