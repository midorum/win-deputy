package midorum.win32.deputy.ui;

import dma.function.SupplierThrowing;
import midorum.win32.deputy.common.CommonUtil;
import midorum.win32.deputy.model.IllegalInputException;
import midorum.win32.deputy.model.Scenario;
import midorum.win32.deputy.model.Displayable;
import midorum.win32.deputy.model.TaskDispatcher;

import javax.swing.*;
import java.awt.*;
import java.io.File;
import java.util.function.Consumer;

class ScenarioEditorForm extends JPanel implements Displayable {

    private static final int PANE_MARGIN = 10;
    private final TaskDispatcher taskDispatcher;
    private final State state;
    private final SupplierThrowing<Scenario, IllegalInputException> scenarioSupplier;
    private final JLabel scenarioPathLabel;

    private ScenarioEditorForm(final TaskDispatcher taskDispatcher, final Scenario scenario) {
        this.taskDispatcher = taskDispatcher;
        this.state = new State(new UiUtil(this));
        final ScenarioEditPane scenarioEditPane = scenario != null ? new ScenarioEditPane(scenario, state) : new ScenarioEditPane(state);
        this.scenarioSupplier = scenarioEditPane;
        scenarioEditPane.setBorder(BorderFactory.createEmptyBorder(0, PANE_MARGIN, PANE_MARGIN, PANE_MARGIN));
        scenarioPathLabel = new JLabel();
        SwingUtil.putComponentsToVerticalGrid(this,
                1,
                scenarioPathLabel,
                scenarioEditPane,
                createButtonPane());
    }

    ScenarioEditorForm(final TaskDispatcher taskDispatcher) {
        this(taskDispatcher, null);
        scenarioPathLabel.setText("New scenario");
    }

    ScenarioEditorForm(final TaskDispatcher taskDispatcher, final Scenario scenario, final File scenarioFile) {
        this(taskDispatcher, scenario);
        state.setWorkingDirectory(scenarioFile.getParentFile());
        state.setScenarioName(scenarioFile.getName());
        scenarioPathLabel.setText(scenarioFile.getAbsolutePath());
    }

    private JPanel createButtonPane() {
        final JPanel buttonPane = new JPanel();
        buttonPane.setLayout(new BoxLayout(buttonPane, BoxLayout.LINE_AXIS));
        buttonPane.setBorder(BorderFactory.createEmptyBorder(0, PANE_MARGIN, PANE_MARGIN, PANE_MARGIN));
        buttonPane.add(Box.createHorizontalGlue());
        buttonPane.add(createSaveButton());
        buttonPane.add(createSaveAsButton());
        buttonPane.add(createSaveAndCloseButton());
        buttonPane.add(createCancelButton());
        return buttonPane;
    }

    private Button createCancelButton() {
        final Button btn = new Button("Cancel");
        btn.addActionListener(e -> taskDispatcher.cancelScenarioEditing());
        return btn;
    }

    private Button createSaveButton() {
        final Button btn = new Button("Save");
        btn.addActionListener(e -> {
            try {
                collectScenarioAndSave(false);
            } catch (IllegalInputException ex) {
                state.getUtilities().reportThrowable(ex.getMessage(), ex);
            }
        });
        return btn;
    }

    private Button createSaveAsButton() {
        final Button btn = new Button("Save As");
        btn.addActionListener(e -> {
            try {
                collectScenarioAndSave(true);
            } catch (IllegalInputException ex) {
                state.getUtilities().reportThrowable(ex.getMessage(), ex);
            }
        });
        return btn;
    }

    private Button createSaveAndCloseButton() {
        final Button btn = new Button("Save and close");
        btn.addActionListener(e -> {
            try {
                collectScenarioAndSave(false);
                if (state.getWorkingDirectory() != null && state.getScenarioName() != null) {
                    taskDispatcher.endScenarioEditing(new File(state.getWorkingDirectory(), state.getScenarioName()).getAbsolutePath());
                }
            } catch (IllegalInputException ex) {
                state.getUtilities().reportThrowable(ex.getMessage(), ex);
            }
        });
        return btn;
    }

    private void collectScenarioAndSave(final boolean askFile) throws IllegalInputException {
        final Scenario scenario = scenarioSupplier.get();
        if (!askFile && state.getWorkingDirectory() != null && state.getScenarioName() != null) {
            saveScenarioFile(scenario, state.getScenarioName());
        } else {
            state.getUtilities().askNewFileAndUpdateState(state, file -> saveScenarioFile(scenario, file.getName()));
        }
    }

    private void saveScenarioFile(final Scenario scenario, final String scenarioName) {
        CommonUtil.saveScenario(scenario, CommonUtil.getPathForScenarios(state.getWorkingDirectory()), scenarioName)
                .consumeOrHandleError((Consumer<? super File>) f -> {
                            final String scenarioFilePath = f.getAbsolutePath();
                            state.setScenarioName(f.getName());
                            scenarioPathLabel.setText(scenarioFilePath);
                            JOptionPane.showMessageDialog(this, "Scenario saved as " + scenarioFilePath);
                        },
                        ex -> state.getUtilities().reportThrowable("Error occurred while saving scenario", ex));
    }

    @Override
    public void display() {
        display(null);
    }

    @Override
    public void display(final String source) {
        setVisible(true);
    }

    @Override
    public void conceal() {
        setVisible(false);
    }

}
