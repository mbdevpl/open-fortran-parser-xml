package fortran.ofp;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

public class ArgsParser {

	private Options options;

	public ArgsParser() {
		options = new Options();

		Option output = new Option(null, "output", true, "output file path, print to System.out if not provided");
		output.setRequired(false);
		options.addOption(output);

		Option verbosity = new Option(null, "verbosity", true, "verbosity level, assume max if not provided");
		options.addOption(verbosity);

		Option help = new Option(null, "help", true, "print this help message and exit");
		help.setRequired(false);
		options.addOption(help);
	}

	public CommandLine parse(String... args) {
		DefaultParser parser = new DefaultParser();
		CommandLine cmd = null;
		try {
			cmd = parser.parse(options, args);
		} catch (ParseException e) {
			System.err.println(e.getMessage());
		}

		if (cmd == null || cmd.hasOption("help")) {
			HelpFormatter formatter = new HelpFormatter();
			formatter.printHelp("fortran.ofp.FrontEnd --class fortran.ofp.XMLPrinter",
					"XML output generator for Open Fortran Parser 0.8.4-1", options,
					"Copyright 2017 Apache License 2.0  Mateusz Bysiek  https://mbdevpl.github.io/", true);
			System.exit(1);
			return null;
		}

		return cmd;
	}

}
