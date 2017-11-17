package fortran.ofp.parser.java;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import org.antlr.runtime.Lexer;
import org.antlr.runtime.Token;

public class CommentTokensList extends ArrayList<Token> {

  protected File file;

  protected FortranStream stream;

  protected FortranCommentLexer lexer;

  protected FortranTokenStream tokenStream;

  public CommentTokensList(File file, boolean fixed_form) throws IOException {
    this.file = file;
    stream = new FortranStream(
      file.getName(), file.getAbsolutePath(),
      fixed_form ? FortranStream.FIXED_FORM : FortranStream.FREE_FORM);
    lexer = new FortranCommentLexer(stream);
    tokenStream = new FortranTokenStream(lexer);

    int i = 0;
    Token token = tokenStream.LT(i);
    while (token != null) {
      add(token);
      ++i;
      token = tokenStream.LT(i);
    }
  }

}
