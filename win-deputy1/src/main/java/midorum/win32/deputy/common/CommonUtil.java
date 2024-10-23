package midorum.win32.deputy.common;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.module.paramnames.ParameterNamesModule;
import com.midorum.win32api.struct.PointInt;
import dma.flow.Either;
import midorum.win32.deputy.model.Scenario;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.time.Instant;
import java.time.ZoneId;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class CommonUtil {

    public static final Pattern POINT_PATTERN = Pattern.compile("^\\((\\d+),(\\d+)\\)$");

    private CommonUtil() {
    }

    public static String trimToNull(final String s) {
        if (s == null) return null;
        final String trim = s.trim();
        return !trim.isBlank() ? trim : null;
    }

    public static <T> int getIndex(T[] array, T t) {
        for (int i = 0; i < array.length; i++)
            if (Objects.equals(array[i], t)) return i;
        return -1;
    }

    public static String localTimeString(final long timestamp) {
        return Instant.ofEpochMilli(timestamp).atZone(ZoneId.systemDefault()).toLocalDateTime().toString();
    }

    public static String utcTimeString(final long timestamp) {
        return Instant.ofEpochMilli(timestamp).atZone(ZoneId.of("UTC")).toLocalDateTime().toString();
    }

    @SafeVarargs
    public static <T> List<T> concatLists(List<T>... lists) {
        int size = 0;
        for (List<T> list : lists) size += list.size();
        final ArrayList<T> ts = new ArrayList<>(size);
        for (List<T> list : lists) ts.addAll(list);
        return Collections.unmodifiableList(ts);
    }

    public static File getWorkingDirectoryForPath(final File file) {
        final String path = file.getAbsolutePath();
        return path.endsWith("shots") ? file.getParentFile() : file;
    }

    public static String getPathForImages(final File workingDirectory) {
        return workingDirectory != null ? workingDirectory.getAbsolutePath() + File.separator + "shots" : "shots";
    }

    public static String getPathForWrongShots(final File workingDirectory) {
        return workingDirectory != null ? workingDirectory.getAbsolutePath() + File.separator + "wrong_shots" : "wrong_shots";
    }

    public static String getPathForScenarios(final File workingDirectory) {
        return workingDirectory != null ? workingDirectory.getAbsolutePath() : "";
    }

    public static String getDefaultFileName() {
        return "_" + System.currentTimeMillis();
    }

    public static String getFileNameForWrongShot() {
        return "shot_" + System.currentTimeMillis();
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
        return Either.valueFrom(() -> new FileServiceProvider()
                        .withFile(path, name, getImageFormat())
                        .writeImage(bufferedImage))
                .orException(() -> new IOException("error while saving image: " + path + File.separator + name));
    }

    public static String getImageFormat() {
        return "png";
    }

    public static Either<File, IOException> saveScenario(final Scenario scenario, final String path, final String name) {
        return Either.valueFrom(() -> new FileServiceProvider()
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
        return Either.valueFrom(() -> objectMapper.readValue(file, Scenario.class))
                .orException(() -> new IOException("error while loading scenario: " + file.getAbsolutePath()));
    }

    public static int[] imageToArray(final BufferedImage image) {
        return image.getRGB(
                image.getMinX(),
                image.getMinY(),
                image.getWidth(),
                image.getHeight(),
                null,
                0,
                image.getWidth());
    }

    public static BufferedImage arrayToImage(final int[] arr, final int width, final int height) {
        final BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        image.setRGB(0, 0, width, height, arr, 0, width);
        return image;
    }

}
