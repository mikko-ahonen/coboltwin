package com.coboltwin.preprocess;

import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class Main {

    public static void main(String[] args) throws Exception {
        if (args.length != 2) {
            System.err.println("Usage: coboltwin-preprocess <input.cbl> <output_core.cbl>");
            System.exit(2);
        }

        File input = new File(args[0]);
        Path outputCore = Path.of(args[1]);

        // Parse with ProLeap. Mainframe sources use FIXED column format.
        Program program = new CobolParserRunnerImpl().analyzeFile(input, CobolSourceFormatEnum.FIXED);

        // ProLeap compilation unit naming is: Capitalized filename without extension.
        String baseName = input.getName();
        String unitName = baseName.contains(".") ? baseName.substring(0, baseName.lastIndexOf('.')) : baseName;
        unitName = unitName.substring(0, 1).toUpperCase() + unitName.substring(1);

        CompilationUnit cu = program.getCompilationUnit(unitName);
        if (cu == null) {
            cu = program.getCompilationUnits().iterator().next();
        }

        @SuppressWarnings("unchecked")
        List<String> lines = (List<String>) cu.getLines();
        String preprocessed = String.join("\n", lines) + "\n";

        String transformed = transform(preprocessed);

        Files.createDirectories(outputCore.getParent());
        Files.writeString(outputCore, transformed, StandardCharsets.UTF_8);

        System.out.println("Wrote core COBOL: " + outputCore);
    }

    private static String transform(String src) {
        // Replace packed decimals with DISPLAY SIGN LEADING SEPARATE so
        // the test wrapper can pass flat-file data with a readable sign character.
        src = src.replaceAll("(?i)\\bCOMP-3\\b", "DISPLAY SIGN LEADING SEPARATE");

        // Replace the mainframe DL/I dispatcher with a test stub call (simple signature)
        src = src.replaceAll("(?i)CALL\\s+['\\\"]CBLTDLI['\\\"]\\s+USING\\s+WS-SEGMENT", "CALL 'DLI-STUB' USING WS-SEGMENT");

        // Defensive: strip embedded SQL if it survived preprocessing (EXEC SQL ... END-EXEC)
        src = src.replaceAll("(?is)\\bEXEC\\s+SQL\\b.*?\\bEND-EXEC\\b\\.?\\s*", "");

        // Defensive: strip SQLCA include (if any)
        src = src.replaceAll("(?is)\\bEXEC\\s+SQL\\s+INCLUDE\\s+SQLCA\\s+END-EXEC\\b\\.?\\s*", "");

        // Strip ProLeap EXEC SQL comment markers
        src = src.replaceAll("(?m)^\\s*\\*>EXECSQL.*$", "");

        // Remove SQLCODE-driven PERFORM loop -- the wrapper calls us once per record
        src = src.replaceAll("(?is)\\bPERFORM\\s+UNTIL\\s+SQLCODE\\s*=\\s*\\d+\\b", "");
        src = src.replaceAll("(?is)\\bEND-PERFORM\\b\\.?", "");

        // Replace STOP RUN with GOBACK (called as subroutine from wrapper)
        src = src.replaceAll("(?i)\\bSTOP\\s+RUN\\b", "GOBACK");

        // Convert WORKING-STORAGE fields to LINKAGE SECTION so the wrapper can
        // pass data in via CALL ... USING.
        src = src.replaceAll("(?i)WORKING-STORAGE SECTION\\.", "LINKAGE SECTION.");
        src = src.replaceAll("(?i)(\\bPROCEDURE DIVISION)\\.",
                "$1 USING WS-CUST-ID WS-NAME WS-BALANCE\n"
              + "                     WS-STATUS WS-SEGMENT.");

        return src;
    }
}
