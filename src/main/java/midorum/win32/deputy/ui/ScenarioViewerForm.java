package midorum.win32.deputy.ui;

import midorum.win32.deputy.model.Scenario;

import javax.swing.*;
import java.awt.*;
import java.io.File;
import java.io.IOException;

public class ScenarioViewerForm extends JPanel implements Displayable {

    private static final int PANE_MARGIN = 10;
    private final TaskDispatcher taskDispatcher;
    private final State state;
    private final JLabel scenarioPathLabel;
    private Button editScenarioButton;

    ScenarioViewerForm(final TaskDispatcher taskDispatcher) {
        this.taskDispatcher = taskDispatcher;
        this.state = new State();
        scenarioPathLabel = new JLabel();
        Util.putComponentsToVerticalGrid(this,
                1,
                scenarioPathLabel,
                new JPanel(),
                createButtonPane());
        scenarioPathLabel.setText("New scenario");
        editScenarioButton.setEnabled(false);
    }

    ScenarioViewerForm(final TaskDispatcher taskDispatcher, final Scenario scenario, final File scenarioFile) {
        this(taskDispatcher);
        state.setWorkingDirectory(scenarioFile.getParentFile());
        state.setScenarioName(scenarioFile.getName());
        scenarioPathLabel.setText(scenarioFile.getAbsolutePath());
        editScenarioButton.setEnabled(true);
    }

    private JPanel createButtonPane() {
        final JPanel buttonPane = new JPanel();
        buttonPane.setLayout(new BoxLayout(buttonPane, BoxLayout.LINE_AXIS));
        buttonPane.setBorder(BorderFactory.createEmptyBorder(0, PANE_MARGIN, PANE_MARGIN, PANE_MARGIN));
        buttonPane.add(Box.createHorizontalGlue());
        buttonPane.add(createLoadScenarioButton());
//        buttonPane.add(Box.createRigidArea(new Dimension(10, 0)));
        editScenarioButton = createEditScenarioButton();
        buttonPane.add(editScenarioButton);
//        buttonPane.add(Box.createRigidArea(new Dimension(10, 0)));
        buttonPane.add(createCreateScenarioButton());
        buttonPane.add(createRunScenarioButton());
        buttonPane.add(createStopScenarioButton());
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
        btn.addActionListener(e -> System.out.println("actionEvent: " + e));
        return btn;
    }

    private Button createStopScenarioButton() {
        final Button btn = new Button("Stop scenario");
        btn.addActionListener(e -> System.out.println("actionEvent: " + e));
        return btn;
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
