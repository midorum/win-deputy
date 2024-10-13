package midorum.win32.deputy.ui;

import midorum.win32.deputy.i18n.I18nResourcesProvider;
import midorum.win32.deputy.i18n.UiElement;
import midorum.win32.deputy.model.LogLevel;
import midorum.win32.deputy.model.Settings;
import midorum.win32.deputy.model.TaskDispatcher;

import javax.swing.*;
import java.awt.*;
import java.io.IOException;

class SettingsPane extends JPanel {

    private static final int PANE_MARGIN = 10;
    private final TaskDispatcher taskDispatcher;
    private final UiUtil uiUtil;
    private IntegerTextField stampDeviationTextField;
    private IntegerTextField maxRepeatableCountTextField;
    private IntegerTextField initialDelayTextField;
    private IntegerTextField delayTextField;
    private IntegerTextField userActivityDelayTextField;
    private JComboBox<String> listAllWindowsWhenSearchFailComboBox;
    private JComboBox<LogLevel> rootLogLevelComboBox;
    private JComboBox<LogLevel> libLogLevelComboBox;
    private JComboBox<I18nResourcesProvider.SupportedLocale> languageComboBox;

    SettingsPane(final TaskDispatcher taskDispatcher, final UiUtil uiUtil) {
        this.taskDispatcher = taskDispatcher;
        this.uiUtil = uiUtil;
        final Settings settings = Settings.loadFromFile().getOrHandleError(e -> {
            uiUtil.reportThrowable(e, UiElement.cannotLoadSettingsFromFile);
            return Settings.defaultSettings();
        });
        SwingUtil.putComponentsToVerticalGrid(this, 1,
                new JLabel(UiElement.settingsLabel.forUserLocale()),
                createContentPane(settings),
                createButtonPane());
    }

    private JPanel createContentPane(final Settings settings) {
        final JPanel panel = new JPanel();
        panel.setBorder(BorderFactory.createEmptyBorder(PANE_MARGIN, PANE_MARGIN, 0, PANE_MARGIN));
        SwingUtil.putComponentsToVerticalGrid(panel, -1,
                createStampDeviationPane(settings.stampDeviation()),
                createMaxRepeatableCountPane(settings.maxRepeatableCount()),
                createInitialDelayPane(settings.initialDelay()),
                createDelayPane(settings.delay()),
                createUserActivityDelayPane(settings.userActivityDelay()),
                createListAllWindowsWhenSearchFailPane(settings.listAllWindowsWhenSearchFail()),
                createRootLogLevelPane(settings.rootLogLevel()),
                createLibLogLevelPane(settings.libLogLevel()),
                createLanguagePane(settings.locale())
        );
        return panel;
    }

    private JPanel createStampDeviationPane(final int value) {
        final JPanel panel = new JPanel();
        stampDeviationTextField = new IntegerTextField(0, Integer.MAX_VALUE, Integer.toString(value));
        SwingUtil.putComponentsToHorizontalGrid(panel, 1,
                new JLabel(UiElement.stampDeviationLabel.forUserLocale()),
                stampDeviationTextField
        );
        return panel;
    }

    private JPanel createMaxRepeatableCountPane(final int value) {
        final JPanel panel = new JPanel();
        maxRepeatableCountTextField = new IntegerTextField(1, Integer.MAX_VALUE, Integer.toString(value));
        SwingUtil.putComponentsToHorizontalGrid(panel, 1,
                new JLabel(UiElement.maxRepeatableCountLabel.forUserLocale()),
                maxRepeatableCountTextField
        );
        return panel;
    }

    private JPanel createInitialDelayPane(final int value) {
        final JPanel panel = new JPanel();
        initialDelayTextField = new IntegerTextField(0, Integer.MAX_VALUE, Integer.toString(value));
        SwingUtil.putComponentsToHorizontalGrid(panel, 1,
                new JLabel(UiElement.initialDelayInSecondsLabel.forUserLocale()),
                initialDelayTextField
        );
        return panel;
    }

    private JPanel createDelayPane(final int value) {
        final JPanel panel = new JPanel();
        delayTextField = new IntegerTextField(1, Integer.MAX_VALUE, Integer.toString(value));
        SwingUtil.putComponentsToHorizontalGrid(panel, 1,
                new JLabel(UiElement.delayBetweenRunsInSecondsLabel.forUserLocale()),
                delayTextField
        );
        return panel;
    }

    private JPanel createUserActivityDelayPane(final int value) {
        final JPanel panel = new JPanel();
        userActivityDelayTextField = new IntegerTextField(1, Integer.MAX_VALUE, Integer.toString(value));
        SwingUtil.putComponentsToHorizontalGrid(panel, 1,
                new JLabel(UiElement.userActivityDelayInSecondsLabel.forUserLocale()),
                userActivityDelayTextField
        );
        return panel;
    }

    private JPanel createListAllWindowsWhenSearchFailPane(final boolean value) {
        final JPanel panel = new JPanel();
        listAllWindowsWhenSearchFailComboBox = new JComboBox<>(new String[]{Boolean.TRUE.toString(), Boolean.FALSE.toString()});
        listAllWindowsWhenSearchFailComboBox.setSelectedItem(Boolean.toString(value));
        SwingUtil.putComponentsToHorizontalGrid(panel, 1,
                new JLabel(UiElement.listAllWindowsWhenSearchFailLabel.forUserLocale()),
                listAllWindowsWhenSearchFailComboBox
        );
        return panel;
    }

    private JPanel createRootLogLevelPane(final LogLevel logLevel) {
        final JPanel panel = new JPanel();
        rootLogLevelComboBox = new JComboBox<>(LogLevel.values());
        rootLogLevelComboBox.setSelectedItem(logLevel);
        SwingUtil.putComponentsToHorizontalGrid(panel, 1,
                new JLabel(UiElement.rootLogLevelLabel.forUserLocale()),
                rootLogLevelComboBox
        );
        return panel;
    }

    private JPanel createLibLogLevelPane(final LogLevel logLevel) {
        final JPanel panel = new JPanel();
        libLogLevelComboBox = new JComboBox<>(LogLevel.values());
        libLogLevelComboBox.setSelectedItem(logLevel);
        SwingUtil.putComponentsToHorizontalGrid(panel, 1,
                new JLabel(UiElement.libLogLevelLabel.forUserLocale()),
                libLogLevelComboBox
        );
        return panel;
    }

    private JPanel createLanguagePane(final I18nResourcesProvider.SupportedLocale locale) {
        final JPanel panel = new JPanel();
        languageComboBox = new JComboBox<>(I18nResourcesProvider.SupportedLocale.values());
        languageComboBox.setSelectedItem(locale);
        SwingUtil.putComponentsToHorizontalGrid(panel, 1,
                new JLabel(UiElement.languageLabel.forUserLocale()),
                languageComboBox
        );
        return panel;
    }

    private JPanel createButtonPane() {
        final JPanel buttonPane = new JPanel();
        buttonPane.setLayout(new BoxLayout(buttonPane, BoxLayout.LINE_AXIS));
        buttonPane.setBorder(BorderFactory.createEmptyBorder(0, PANE_MARGIN, PANE_MARGIN, PANE_MARGIN));
        buttonPane.add(Box.createHorizontalGlue());
        buttonPane.add(createSaveButton());
        buttonPane.add(createCloseButton());
        return buttonPane;
    }

    private Button createSaveButton() {
        final Button btn = new Button(UiElement.applyButtonText.forUserLocale());
        btn.addActionListener(_ -> {
            try {
                final Settings settings = new Settings(
                        Integer.parseInt(stampDeviationTextField.getText()),
                        Integer.parseInt(maxRepeatableCountTextField.getText()),
                        Integer.parseInt(initialDelayTextField.getText()),
                        Integer.parseInt(delayTextField.getText()),
                        Integer.parseInt(userActivityDelayTextField.getText()),
                        Boolean.parseBoolean((String) listAllWindowsWhenSearchFailComboBox.getSelectedItem()),
                        getSelectedRootLogLevel(),
                        getSelectedLibLogLevel(),
                        getSelectedLocale()
                );
                settings.storeToFile();
                uiUtil.getLogger().info("application settings updated: {}", settings);
                taskDispatcher.applySettings();
            } catch (IOException ex) {
                uiUtil.reportThrowable(ex, UiElement.cannotStoreSettingsToFile);
            }
        });
        return btn;
    }

    private LogLevel getSelectedRootLogLevel() {
        final Object selectedRootLogLevelValue = rootLogLevelComboBox.getSelectedItem();
        return selectedRootLogLevelValue != null ? (LogLevel) selectedRootLogLevelValue : LogLevel.INFO;
    }

    private LogLevel getSelectedLibLogLevel() {
        final Object selectedLibLogLevelValue = libLogLevelComboBox.getSelectedItem();
        return selectedLibLogLevelValue != null ? (LogLevel) selectedLibLogLevelValue : LogLevel.INFO;
    }

    private I18nResourcesProvider.SupportedLocale getSelectedLocale() {
        final Object selectedLanguage = languageComboBox.getSelectedItem();
        return selectedLanguage != null
                ? (I18nResourcesProvider.SupportedLocale) selectedLanguage : I18nResourcesProvider.DEFAULT_LOCALE;
    }

    private Button createCloseButton() {
        final Button btn = new Button(UiElement.closeButtonText.forUserLocale());
        btn.addActionListener(e -> taskDispatcher.closeSettings());
        return btn;
    }

}
