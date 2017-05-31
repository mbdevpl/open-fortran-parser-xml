package fortran.ofp;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

public class Main {

	public static void main(String args[]) throws IOException {
		Options options = new Options();
		options.addOption(null, "RiceCAF", false, "use Rice University's CAF extensions");
		options.addOption(null, "LOPExt", false, "use OFP's LOPe research extensions");

		Option input = new Option("f", "file", true, "input file path");
		input.setRequired(true);
		options.addOption(input);

		Option verbose = new Option("v", false, "verbose");
		options.addOption(verbose);

		Option output = new Option("o", "output", true, "output file");
		output.setRequired(false);
		options.addOption(output);

		DefaultParser parser = new DefaultParser();
		CommandLine cmd = null;
		try {
			cmd = parser.parse(options, args);
		} catch (ParseException e) {
			System.out.println(e.getMessage());
		}

		if (cmd == null || cmd.hasOption("help")) {
			HelpFormatter formatter = new HelpFormatter();
			formatter.printHelp("open-fortran-parser-xml [options] file", options);
			System.exit(1);
			return;
		}

		System.out.println(cmd.getArgList().toString());
		System.out.println(Arrays.asList(cmd.getOptions()).toString());

		String inputFilePath = cmd.getOptionValue("file");
		String outputFilePath = cmd.getOptionValue("output");

		System.out.println(inputFilePath);
		System.out.println(outputFilePath);

		List<String> ofpArgs = new ArrayList<String>();
		if (cmd.hasOption("RiceCAF"))
			ofpArgs.add("--RiceCAF");
		if (cmd.hasOption("LOPExt"))
			ofpArgs.add("--LOPExt");
		String[] ofpArgsArray = ofpArgs.toArray(new String[ofpArgs.size()]);

		String type = "fortran.ofp.XMLPrinter";
		FrontEnd ofp = new FrontEnd(ofpArgsArray, cmd.getOptionValue("file"), type);
		ofp.setVerbose(cmd.hasOption("verbose"), null);
		// TODO: incomplete implementation
	}

}
