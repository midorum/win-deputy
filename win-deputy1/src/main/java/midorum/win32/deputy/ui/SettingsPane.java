package midorum.win32.deputy.ui;

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
                createListAllWindowsWhenSearchFailPane(settings.listAllWindowsWhenSearchFail())
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
        btn.addActionListener(e -> {
            try {
                new Settings(
                        Integer.parseInt(stampDeviationTextField.getText()),
                        Integer.parseInt(maxRepeatableCountTextField.getText()),
                        Integer.parseInt(initialDelayTextField.getText()),
                        Integer.parseInt(delayTextField.getText()),
                        Integer.parseInt(userActivityDelayTextField.getText()),
                        Boolean.parseBoolean((String) listAllWindowsWhenSearchFailComboBox.getSelectedItem())
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
