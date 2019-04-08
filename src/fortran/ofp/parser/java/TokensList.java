package fortran.ofp.parser.java;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.antlr.runtime.Token;

public class TokensList extends ArrayList<Token> {

	private static final Logger LOG = Logger.getLogger(TokensList.class.getName());

	private static final long serialVersionUID = -8037754729217056476L;

	public TokensList(File file) throws IOException {
		addAll(file, null);
	}

	public TokensList(File file, int onlyOfType) throws IOException {
		addAll(file, onlyOfType);
	}

	public void addAll(File file) throws IOException {
		addAll(file, null);
	}

	public void addAll(File file, Integer onlyOfType) throws IOException {
		FortranStream stream = new FortranStream(file.getName(), file.getAbsolutePath(), null);
		FortranAlternateLexer lexer = new FortranAlternateLexer(stream);

		Token token = lexer.nextToken();
		while (token.getType() != FortranAlternateLexer.EOF) {
			if (onlyOfType == null || token.getType() == ((int) onlyOfType))
				add(token);
			// else LOG.log(Level.INFO, "skipping token of type {0}", token.getType());
			token = lexer.nextToken();
		}
	}

}
