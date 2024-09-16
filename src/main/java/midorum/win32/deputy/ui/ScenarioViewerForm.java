package midorum.win32.deputy.ui;

import midorum.win32.deputy.executor.ExecutorImpl;
import midorum.win32.deputy.model.Displayable;
import midorum.win32.deputy.model.Scenario;
import midorum.win32.deputy.model.TaskDispatcher;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.awt.*;
import java.io.File;

public class ScenarioViewerForm extends JPanel implements Displayable {

    private static final int PANE_MARGIN = 10;
    private final Logger logger = LogManager.getLogger(this);
    private final TaskDispatcher taskDispatcher;
    private final State state;
    private final JLabel scenarioPathLabel;
    private Button editScenarioButton;
    private Button runScenarioButton;
    private Button stopScenarioButton;
    private final JLabel scenarioTitleLabel;
    private final JLabel scenarioDescriptionLabel;
    private Scenario scenario;
    private final ExecutorImpl executor;
    private Button loadScenarioButton;
    private Button createScenarioButton;

    ScenarioViewerForm(final TaskDispatcher taskDispatcher) {
        this.taskDispatcher = taskDispatcher;
        this.state = new State();
        this.executor = new ExecutorImpl();
        this.scenarioPathLabel = new JLabel();
        this.scenarioTitleLabel = new JLabel();
        this.scenarioDescriptionLabel = new JLabel();
        Util.putComponentsToVerticalGrid(this,
                3,
                scenarioPathLabel,
                scenarioTitleLabel,
                scenarioDescriptionLabel,
                new JPanel(),
                createButtonPane());
        scenarioPathLabel.setText("New scenario");
        editScenarioButton.setEnabled(false);
        runScenarioButton.setEnabled(false);
        stopScenarioButton.setEnabled(false);
    }

    ScenarioViewerForm(final TaskDispatcher taskDispatcher, final Scenario scenario, final File scenarioFile) {
        this(taskDispatcher);
        this.scenario = scenario;
        state.setWorkingDirectory(scenarioFile.getParentFile());
        state.setScenarioName(scenarioFile.getName());
        scenarioPathLabel.setText(scenarioFile.getAbsolutePath());
        scenarioTitleLabel.setText(scenario.getTitle());
        scenario.getDescription().ifPresent(scenarioDescriptionLabel::setText);
        editScenarioButton.setEnabled(true);
        runScenarioButton.setEnabled(true);
    }

    private JPanel createButtonPane() {
        final JPanel buttonPane = new JPanel();
        buttonPane.setLayout(new BoxLayout(buttonPane, BoxLayout.LINE_AXIS));
        buttonPane.setBorder(BorderFactory.createEmptyBorder(0, PANE_MARGIN, PANE_MARGIN, PANE_MARGIN));
        buttonPane.add(Box.createHorizontalGlue());
        loadScenarioButton = createLoadScenarioButton();
        buttonPane.add(loadScenarioButton);
        createScenarioButton = createCreateScenarioButton();
        buttonPane.add(createScenarioButton);
        editScenarioButton = createEditScenarioButton();
        buttonPane.add(editScenarioButton);
        runScenarioButton = createRunScenarioButton();
        buttonPane.add(runScenarioButton);
        stopScenarioButton = createStopScenarioButton();
        buttonPane.add(stopScenarioButton);
        return buttonPane;
    }

    private Button createLoadScenarioButton() {
        final Button btn = new Button("Load scenario");
        btn.addActionListener(e -> taskDispatcher.loadScenario());
        return btn;
    }

    private Button createEditScenarioButton() {
        final Button btn = new Button("Edit scenario");
        btn.addActionListener(e -> taskDispatcher.editScenario(new File(state.getWorkingDirectory(),
                state.getScenarioName()).getAbsolutePath()));
        return btn;
    }

    private Button createCreateScenarioButton() {
        final Button btn = new Button("Create scenario");
        btn.addActionListener(e -> taskDispatcher.createNewScenario());
        return btn;
    }

    private Button createRunScenarioButton() {
        final Button btn = new Button("Run scenario");
        btn.addActionListener(e -> {
            if (scenario != null) {
                lockForm();
                executor.sendRoutineTask(scenario, throwable -> {
                    final String message = "Error occurred while executing scenario";
                    logger.error(message, throwable);
                    JOptionPane.showMessageDialog(this, message);
                });
            }
        });
        return btn;
    }

    private Button createStopScenarioButton() {
        final Button btn = new Button("Stop scenario");
        btn.addActionListener(e -> {
            executor.cancelCurrentTask();
            unlockForm();
        });
        return btn;
    }

    private void lockForm() {
        loadScenarioButton.setEnabled(false);
        editScenarioButton.setEnabled(false);
        createScenarioButton.setEnabled(false);
        runScenarioButton.setEnabled(false);
        stopScenarioButton.setEnabled(true);
    }

    private void unlockForm() {
        loadScenarioButton.setEnabled(true);
        editScenarioButton.setEnabled(true);
        createScenarioButton.setEnabled(true);
        runScenarioButton.setEnabled(true);
        stopScenarioButton.setEnabled(false);
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
