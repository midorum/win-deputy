package midorum.win32.deputy.model;

import dma.file.FileInputStream;
import dma.file.FileOutputStream;
import midorum.win32.deputy.common.Either;

import java.io.IOException;
import java.util.Properties;

public record Settings(
        int stampDeviation,
        int maxRepeatableCount,
        int initialDelay,
        int delay,
        int userActivityDelay,
        boolean listAllWindowsWhenSearchFail,
        LogLevel rootLogLevel,
        LogLevel libLogLevel
) {

    private static final String SETTINGS_FILE_NAME = "settings";
    private static final String STAMP_DEVIATION_PROPERTY_NAME = "stamp.deviation";
    private static final String MAX_REPEATABLE_COUNT_PROPERTY_NAME = "repeatable.count.max";
    private static final String INITIAL_DELAY_PROPERTY_NAME = "initialDelay";
    private static final String DELAY_PROPERTY_NAME = "delay";
    private static final String USER_ACTIVITY_DELAY_PROPERTY_NAME = "userActivityDelay";
    private static final String LIST_ALL_WINDOWS_WHEN_SEARCH_FAIL_PROPERTY_NAME = "search.window.listAllWhenFail";
    private static final String ROOT_LOG_LEVEL_PROPERTY_NAME = "log.root.level";
    private static final String LIB_LOG_LEVEL_PROPERTY_NAME = "log.lib.level";

    public static Settings defaultSettings() {
        return new Settings(
                1,
                3,
                5,
                1,
                15,
                false,
                LogLevel.INFO,
                LogLevel.INFO
        );
    }

    public static Either<Settings, IOException> loadFromFile() {
        return Either.value(() -> FileInputStream.getInstance().useWithInputStream(SETTINGS_FILE_NAME, inputStream -> {
            Properties appProps = new Properties();
            appProps.load(inputStream);
            return new Settings(
                    Integer.parseInt(appProps.getProperty(STAMP_DEVIATION_PROPERTY_NAME, "1")),
                    Integer.parseInt(appProps.getProperty(MAX_REPEATABLE_COUNT_PROPERTY_NAME, "3")),
                    Integer.parseInt(appProps.getProperty(INITIAL_DELAY_PROPERTY_NAME, "5")),
                    Integer.parseInt(appProps.getProperty(DELAY_PROPERTY_NAME, "1")),
                    Integer.parseInt(appProps.getProperty(USER_ACTIVITY_DELAY_PROPERTY_NAME, "15")),
                    Boolean.parseBoolean(appProps.getProperty(LIST_ALL_WINDOWS_WHEN_SEARCH_FAIL_PROPERTY_NAME, "false")),
                    LogLevel.valueOf(appProps.getProperty(ROOT_LOG_LEVEL_PROPERTY_NAME, LogLevel.INFO.name())),
                    LogLevel.valueOf(appProps.getProperty(LIB_LOG_LEVEL_PROPERTY_NAME, LogLevel.INFO.name()))
            );
        })).orException();
    }

    public void storeToFile() throws IOException {
        FileOutputStream.getInstance().rewriteWithOutputStream(SETTINGS_FILE_NAME, outputStream -> {
            Properties appProps = new Properties();
            appProps.setProperty(STAMP_DEVIATION_PROPERTY_NAME, Integer.toString(stampDeviation()));
            appProps.setProperty(MAX_REPEATABLE_COUNT_PROPERTY_NAME, Integer.toString(maxRepeatableCount()));
            appProps.setProperty(INITIAL_DELAY_PROPERTY_NAME, Integer.toString(initialDelay()));
            appProps.setProperty(DELAY_PROPERTY_NAME, Integer.toString(delay()));
            appProps.setProperty(USER_ACTIVITY_DELAY_PROPERTY_NAME, Integer.toString(userActivityDelay()));
            appProps.setProperty(LIST_ALL_WINDOWS_WHEN_SEARCH_FAIL_PROPERTY_NAME, Boolean.toString(listAllWindowsWhenSearchFail()));
            appProps.setProperty(ROOT_LOG_LEVEL_PROPERTY_NAME, rootLogLevel.name());
            appProps.setProperty(LIB_LOG_LEVEL_PROPERTY_NAME, libLogLevel.name());
            appProps.store(outputStream, null);
        });
    }
}
