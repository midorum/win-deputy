package midorum.win32.deputy.ui;

import midorum.win32.deputy.model.Scenario;

import javax.swing.*;
import java.awt.*;

public class ScenarioEditorForm extends JPanel implements Displayable {

    private static final int PANE_MARGIN = 10;
    private final TaskDispatcher taskDispatcher;

    public ScenarioEditorForm(final TaskDispatcher taskDispatcher) {
        this.taskDispatcher = taskDispatcher;
        Util.putComponentsToVerticalGrid(this,
                new JLabel("Scenario path will be here"),
                createScenarioPane(),
                createButtonPane());
    }

    private JPanel createScenarioPane() {
        final JPanel scenarioPane = new ScenarioEditPane();
        scenarioPane.setBorder(BorderFactory.createEmptyBorder(0, PANE_MARGIN, PANE_MARGIN, PANE_MARGIN));
        return scenarioPane;
    }

    private JPanel createButtonPane() {
        final JPanel buttonPane = new JPanel();
        buttonPane.setLayout(new BoxLayout(buttonPane, BoxLayout.LINE_AXIS));
        buttonPane.setBorder(BorderFactory.createEmptyBorder(0, PANE_MARGIN, PANE_MARGIN, PANE_MARGIN));
        buttonPane.add(Box.createHorizontalGlue());
        buttonPane.add(createCancelButton());
        buttonPane.add(createSaveButton());
        buttonPane.add(createSaveAndCloseButton());
        return buttonPane;
    }

    private Button createCancelButton() {
        final Button btn = new Button("Cancel");
        btn.addActionListener(e -> taskDispatcher.cancelScenarioEditing());
        return btn;
    }

    private Button createSaveButton() {
        final Button btn = new Button("Save");
        btn.addActionListener(e -> System.out.println(e));
        return btn;
    }

    private Button createSaveAndCloseButton() {
        final Button btn = new Button("Save and close");
        btn.addActionListener(e -> System.out.println(e));
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
