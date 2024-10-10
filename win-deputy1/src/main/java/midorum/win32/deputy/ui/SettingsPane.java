package midorum.win32.deputy.ui;

import midorum.win32.deputy.model.LogLevel;
import midorum.win32.deputy.model.Settings;
import midorum.win32.deputy.model.TaskDispatcher;
import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.core.config.Configurator;

import javax.swing.*;
import java.awt.*;
import java.io.IOException;
import java.util.Objects;

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

    SettingsPane(final TaskDispatcher taskDispatcher, final UiUtil uiUtil) {
        this.taskDispatcher = taskDispatcher;
        this.uiUtil = uiUtil;
        final Settings settings = Settings.loadFromFile().getOrHandleError(e -> {
            uiUtil.reportThrowable("Cannot load settings from file", e);
            return Settings.defaultSettings();
        });
        SwingUtil.putComponentsToVerticalGrid(this, 1,
                new JLabel("Settings"),
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
                createLibLogLevelPane(settings.libLogLevel())
        );
        return panel;
    }

    private JPanel createStampDeviationPane(final int value) {
        final JPanel panel = new JPanel();
        stampDeviationTextField = new IntegerTextField(0, Integer.MAX_VALUE, Integer.toString(value));
        SwingUtil.putComponentsToHorizontalGrid(panel, 1,
                new JLabel("Stamp deviation:"),
                stampDeviationTextField
        );
        return panel;
    }

    private JPanel createMaxRepeatableCountPane(final int value) {
        final JPanel panel = new JPanel();
        maxRepeatableCountTextField = new IntegerTextField(1, Integer.MAX_VALUE, Integer.toString(value));
        SwingUtil.putComponentsToHorizontalGrid(panel, 1,
                new JLabel("Max repeatable count:"),
                maxRepeatableCountTextField
        );
        return panel;
    }

    private JPanel createInitialDelayPane(final int value) {
        final JPanel panel = new JPanel();
        initialDelayTextField = new IntegerTextField(0, Integer.MAX_VALUE, Integer.toString(value));
        SwingUtil.putComponentsToHorizontalGrid(panel, 1,
                new JLabel("Initial delay, seconds:"),
                initialDelayTextField
        );
        return panel;
    }

    private JPanel createDelayPane(final int value) {
        final JPanel panel = new JPanel();
        delayTextField = new IntegerTextField(1, Integer.MAX_VALUE, Integer.toString(value));
        SwingUtil.putComponentsToHorizontalGrid(panel, 1,
                new JLabel("Delay between runs, seconds:"),
                delayTextField
        );
        return panel;
    }

    private JPanel createUserActivityDelayPane(final int value) {
        final JPanel panel = new JPanel();
        userActivityDelayTextField = new IntegerTextField(1, Integer.MAX_VALUE, Integer.toString(value));
        SwingUtil.putComponentsToHorizontalGrid(panel, 1,
                new JLabel("User activity delay, seconds:"),
                userActivityDelayTextField
        );
        return panel;
    }

    private JPanel createListAllWindowsWhenSearchFailPane(final boolean value) {
        final JPanel panel = new JPanel();
        listAllWindowsWhenSearchFailComboBox = new JComboBox<>(new String[]{Boolean.TRUE.toString(), Boolean.FALSE.toString()});
        listAllWindowsWhenSearchFailComboBox.setSelectedItem(Boolean.toString(value));
        SwingUtil.putComponentsToHorizontalGrid(panel, 1,
                new JLabel("List all windows when search fail:"),
                listAllWindowsWhenSearchFailComboBox
        );
        return panel;
    }

    private JPanel createRootLogLevelPane(final LogLevel logLevel) {
        final JPanel panel = new JPanel();
        rootLogLevelComboBox = new JComboBox<>(LogLevel.values());
        rootLogLevelComboBox.setSelectedItem(logLevel);
        rootLogLevelComboBox.addActionListener(_ -> {
            final LogLevel selected = (LogLevel) rootLogLevelComboBox.getSelectedItem();
            if (selected != null) LogLevel.setRootLevel(selected);
        });
        SwingUtil.putComponentsToHorizontalGrid(panel, 1,
                new JLabel("Root log level:"),
                rootLogLevelComboBox
        );
        return panel;
    }

    private JPanel createLibLogLevelPane(final LogLevel logLevel) {
        final JPanel panel = new JPanel();
        libLogLevelComboBox = new JComboBox<>(LogLevel.values());
        libLogLevelComboBox.setSelectedItem(logLevel);
        libLogLevelComboBox.addActionListener(_ -> {
            final LogLevel selected = (LogLevel) libLogLevelComboBox.getSelectedItem();
            if (selected != null) LogLevel.setLibLevel(selected);
        });
        SwingUtil.putComponentsToHorizontalGrid(panel, 1,
                new JLabel("Lib log level:"),
                libLogLevelComboBox
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
        final Button btn = new Button("Save changes");
        btn.addActionListener(_ -> {
            try {
                new Settings(
                        Integer.parseInt(stampDeviationTextField.getText()),
                        Integer.parseInt(maxRepeatableCountTextField.getText()),
                        Integer.parseInt(initialDelayTextField.getText()),
                        Integer.parseInt(delayTextField.getText()),
                        Integer.parseInt(userActivityDelayTextField.getText()),
                        Boolean.parseBoolean((String) listAllWindowsWhenSearchFailComboBox.getSelectedItem()),
                        LogLevel.valueOf(((LogLevel) Objects.requireNonNull(rootLogLevelComboBox.getSelectedItem())).name()),
                        LogLevel.valueOf(((LogLevel) Objects.requireNonNull(libLogLevelComboBox.getSelectedItem())).name())
                ).storeToFile();
            } catch (IOException ex) {
                uiUtil.reportThrowable("Cannot store settings to a file", ex);
            }
        });
        return btn;
    }

    private Button createCloseButton() {
        final Button btn = new Button("Close");
        btn.addActionListener(e -> taskDispatcher.closeSettings());
        return btn;
    }

}
