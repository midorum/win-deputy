package midorum.win32.deputy.common;

import java.io.File;
import java.time.Instant;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

public final class CommonUtil {

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

}
