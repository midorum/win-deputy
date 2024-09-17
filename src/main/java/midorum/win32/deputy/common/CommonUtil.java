package midorum.win32.deputy.common;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.module.paramnames.ParameterNamesModule;
import com.midorum.win32api.struct.PointInt;
import midorum.win32.deputy.model.Scenario;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Objects;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class CommonUtil {

    public static final Pattern POINT_PATTERN = Pattern.compile("^\\((\\d+),(\\d+)\\)$");

    private CommonUtil() {
    }

    public static <T> int getIndex(T[] array, T t) {
        for (int i = 0; i < array.length; i++)
            if (Objects.equals(array[i], t)) return i;
        return -1;
    }

    public static File getWorkingDirectoryForPath(final File file) {
        final String path = file.getAbsolutePath();
        return path.endsWith("shots") ? file.getParentFile() : file;
    }

    public static String getPathForImages(final File workingDirectory) {
        return workingDirectory != null ? workingDirectory.getAbsolutePath() + File.separator + "shots" : "shots";
    }

    public static String getPathForScenarios(final File workingDirectory) {
        return workingDirectory != null ? workingDirectory.getAbsolutePath() : "";
    }

    public static String getDefaultFileName() {
        return "_" + System.currentTimeMillis();
    }

    public static String pointToString(final PointInt point) {
        return "(" + point.x() + "," + point.y() + ")";
    }

    public static Optional<PointInt> stringToPoint(final String s) {
        if (s == null) return Optional.empty();
        final Matcher matcher = POINT_PATTERN.matcher(s);
        if (!matcher.matches()) return Optional.empty();
        return Optional.of(new PointInt(Integer.parseInt(matcher.group(1)), Integer.parseInt(matcher.group(2))));
    }

    public static Either<File, IOException> saveImage(final BufferedImage bufferedImage, final String path, final String name) {
        return Either.value(() -> new FileServiceProvider()
                        .withFile(path, name, "png")
                        .writeImage(bufferedImage))
                .orException(() -> new IOException("error while saving image: " + path + File.separator + name));
    }

    public static Either<File, IOException> saveScenario(final Scenario scenario, final String path, final String name) {
        return Either.value(() -> new FileServiceProvider()
                        .withFile(path, name, "wds")
                        .writeJson(scenario))
                .orException(() -> new IOException("error while saving scenario: " + path + File.separator + name));
    }

    public static Either<Scenario, IOException> loadScenario(final File file) {
        final ObjectMapper objectMapper = new ObjectMapper()
                .registerModule(new ParameterNamesModule())
                .registerModule(new Jdk8Module())
                .registerModule(new JavaTimeModule());
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        return Either.value(() -> objectMapper.readValue(file, Scenario.class))
                .orException(() -> new IOException("error while loading scenario: " + file.getAbsolutePath()));
    }

}
