package midorum.win32.deputy.ui;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Objects;

public class FileServiceProvider {

    FileServiceProvider() {
    }

    public FileService withFile(final String name, final String format) throws IOException {
        return new FileService(Objects.requireNonNull(name, "file name cannot be null"), Objects.requireNonNull(format, "file format cannot be null"));
    }

    public static class FileService {
        private final String format;
        private final File file;

        private FileService(final String name, final String format) throws IOException {
            this.format = format;
            this.file = new File(name + "." + format);
            if (!file.mkdirs()) throw new IOException("Cannot create directories: " + file.getAbsolutePath());
        }

        public boolean writeImage(final BufferedImage image) throws IOException {
            return ImageIO.write(image, format, file);
        }

        public String getFileName() {
            return file.getName();
        }
    }
}
