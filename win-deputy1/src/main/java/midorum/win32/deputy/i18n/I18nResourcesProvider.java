package midorum.win32.deputy.i18n;

import dma.flow.Either;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.AbstractMap;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicReference;

public class I18nResourcesProvider {

    public enum SupportedLocale {
        enUs, ruRu
    }

    public static final SupportedLocale DEFAULT_LOCALE = SupportedLocale.enUs;
    private static final I18nResourcesProvider INSTANCE = new I18nResourcesProvider();
    private final Logger logger = LogManager.getLogger("ui");
    private final Properties defaultLocaleValues;
    private final AtomicReference<SupportedLocale> userLocale = new AtomicReference<>();
    private final AtomicReference<Properties> userLocaleValues = new AtomicReference<>();

    private I18nResourcesProvider() {
        logger.error(">>> I18nResourcesProvider init");
        defaultLocaleValues = loadFromFile(DEFAULT_LOCALE).getOrHandleError(e -> {
            logger.error("cannot load default locale", e);
            return new Properties();
        });
        logger.error(">>> I18nResourcesProvider default locale loaded");
        userLocaleValues.set(defaultLocaleValues);
    }

    public static I18nResourcesProvider getInstance() {
        return INSTANCE;
    }

    public Optional<String> forDefaultLocale(final String key) {
        return Optional.ofNullable(defaultLocaleValues.getProperty(key));
    }

    public Optional<String> forUserLocale(final String key) {
        return Optional.ofNullable(userLocaleValues.get().getProperty(key));
    }

    public SupportedLocale getUserLocale() {
        return userLocale.get();
    }

    public void setUserLocale(final SupportedLocale locale) {
        final AbstractMap.SimpleImmutableEntry<SupportedLocale, Properties> loaded = DEFAULT_LOCALE == locale
                ? new AbstractMap.SimpleImmutableEntry<>(DEFAULT_LOCALE, defaultLocaleValues)
                : loadFromFile(locale)
                .map(properties -> new AbstractMap.SimpleImmutableEntry<>(locale, properties))
                .getOrHandleError(e -> {
                    logger.error(() -> "cannot load locale " + locale, e);
                    return new AbstractMap.SimpleImmutableEntry<>(DEFAULT_LOCALE, defaultLocaleValues);
                });
        synchronized (this) {
            userLocale.set(loaded.getKey());
            userLocaleValues.set(loaded.getValue());
        }
    }

    private Either<Properties, IOException> loadFromFile(final SupportedLocale locale) {
        return Either.valueFrom(() -> {
            final String resourcePath = "/l10n/" + locale.name();
            try (final InputStream is = getClass().getResourceAsStream(resourcePath);
                 final Reader reader = new InputStreamReader(
                         Objects.requireNonNull(is, () -> "cannot obtain resource: " + resourcePath),
                         StandardCharsets.UTF_8)) {
                final Properties properties = new Properties();
                properties.load(reader);
                return properties;
            }
        }).orException();
    }

}
