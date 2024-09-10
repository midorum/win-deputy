package midorum.win32.deputy.ui;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Objects;

class FileServiceProvider {

    FileServiceProvider() {
    }

    public FileService withFile(final String path, final String name, final String format) throws IOException {
        return new FileService(
                path,
                Objects.requireNonNull(name, "file name cannot be null"),
                Objects.requireNonNull(format, "file format cannot be null"));
    }

    public FileService withFile(final String name, final String format) throws IOException {
        return withFile(null, name, format);
    }

    public static class FileService {
        private final String format;
        private final File file;

        private FileService(final String path, final String name, final String format) throws IOException {
            this.format = format;
            final String safeName = name.replaceAll("[<>:\"/\\\\|?*\s]", "_");
            final String fileExtension = "." + format;
            this.file = new File((path != null ? path + File.separator : "")
                    + safeName
                    + (safeName.endsWith(fileExtension) ? "" : fileExtension));
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
