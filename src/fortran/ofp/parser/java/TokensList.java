package fortran.ofp.parser.java;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import org.antlr.runtime.Lexer;
import org.antlr.runtime.Token;

/*
class FortranAltStream extends FortranStream {

  public FortranAltStream(String filename, String path, int sourceForm) throws IOException {
    super(filename, path, sourceForm);
  }

  public char[] getData() {
    return data;
  }
}
*/

public class TokensList extends ArrayList<Token> {

  public TokensList(File file, boolean fixed_form) throws IOException {
    addAll(file, fixed_form);
  }

  public TokensList(File file, boolean fixed_form, int onlyOfType) throws IOException {
    addAll(file, fixed_form, onlyOfType);
  }

  public void addAll(File file, boolean fixed_form) throws IOException {
    addAll(file, fixed_form, null);
  }

  public void addAll(File file, boolean fixed_form, Integer onlyOfType) throws IOException {
    FortranStream stream = new FortranStream(
      file.getName(), file.getAbsolutePath(),
      fixed_form ? FortranStream.FIXED_FORM : FortranStream.FREE_FORM);
    // System.err.println(stream.getData());
    FortranAlternateLexer lexer = new FortranAlternateLexer(stream);

    Token token = lexer.nextToken();
    while(token.getType() != FortranAlternateLexer.EOF) {
      if (onlyOfType == null || token.getType() == ((int) onlyOfType))
        add(token);
      token = lexer.nextToken();
    }
  }

}
