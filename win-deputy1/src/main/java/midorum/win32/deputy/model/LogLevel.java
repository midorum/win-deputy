package midorum.win32.deputy.model;

import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.core.config.Configurator;

public enum LogLevel {
    INFO,
    DEBUG,
    TRACE,
    ALL;

    public static void setRootLevel(final LogLevel logLevel) {
        Configurator.setRootLevel(Level.valueOf(logLevel.name()));
    }

    public static void setLibLevel(final LogLevel logLevel) {
        Configurator.setLevel("com.midorum.win32api", Level.valueOf(logLevel.name()));
    }

}
