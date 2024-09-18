package midorum.win32.deputy.common;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.module.paramnames.ParameterNamesModule;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Objects;

public class FileServiceProvider {

    public FileServiceProvider() {
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
            if (!file.getParentFile().exists() && !file.mkdirs())
                throw new IOException("Cannot create directories: " + file.getAbsolutePath());
        }

        public File writeImage(final BufferedImage image) throws IOException {
            ImageIO.write(image, format, file);
            return file;
        }

        public BufferedImage readImage() throws IOException {
            return ImageIO.read(file);
        }

        public File writeJson(final Object object) throws IOException {
            final ObjectMapper objectMapper = new ObjectMapper()
                    .registerModule(new ParameterNamesModule())
                    .registerModule(new Jdk8Module())
                    .registerModule(new JavaTimeModule());
            objectMapper.writeValue(file, object);
            return file;
        }

        public String getFileName() {
            return file.getName();
        }
    }
}
