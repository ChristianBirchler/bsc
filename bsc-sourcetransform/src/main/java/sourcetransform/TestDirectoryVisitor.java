package sourcetransform;

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;

public class TestDirectoryVisitor implements FileVisitor<Path> {
    @Override
    public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException {
        // avoid traversing src/main/
        if (dir.getFileName().toString().equals("main") ||
                dir.getFileName().toString().equals(".idea") ||
                dir.getFileName().toString().equals(".git")) {
            return FileVisitResult.SKIP_SUBTREE;
        }
        return FileVisitResult.CONTINUE;
    }

    @Override
    public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
        if (file.toString().endsWith(".java")) {
            System.out.println("Inject measurement code to: ");
            System.out.println(file.toString());
            PerformanceMetricsInjector pmi = new PerformanceMetricsInjector(file.toFile());
            pmi.injectMeasurementCode();

            //pmi.writeToConsole();
            pmi.writeToFile();
        }
        return FileVisitResult.CONTINUE;
    }

    @Override
    public FileVisitResult visitFileFailed(Path file, IOException exc) throws IOException {
        return FileVisitResult.CONTINUE;
    }

    @Override
    public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
        return FileVisitResult.CONTINUE;
    }
}
