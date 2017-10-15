package fortran.ofp;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

public class XmlPrinterArgsParser {

	private Options options;

	public XmlPrinterArgsParser() {
		options = new Options();

		Option output = new Option(null, "output", true, "output file path, print to System.out if not provided");
		output.setRequired(false);
		options.addOption(output);

		Option verbosity = new Option(null, "verbosity", true, "verbosity level, assume max if not provided");
		options.addOption(verbosity);
	}

	public CommandLine parse(String... args) {
		DefaultParser parser = new DefaultParser();
		CommandLine cmd = null;
		try {
			cmd = parser.parse(options, args);
		} catch (ParseException e) {
			System.err.println(e.getMessage());
			HelpFormatter formatter = new HelpFormatter();
			formatter.printHelp("fortran.ofp.FrontEnd --class fortran.ofp.XMLPrinter",
					"XML output generator for Open Fortran Parser", options,
					"Copyright 2017 Apache License 2.0  Mateusz Bysiek  https://mbdevpl.github.io/", true);
		}

		return cmd;
	}

}
