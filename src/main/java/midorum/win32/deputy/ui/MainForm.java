package midorum.win32.deputy.ui;

import java.awt.*;

import javax.swing.*;

public class MainForm implements TaskDispatcher {

    private final JFrame frame;
    private final ScenarioViewerForm scenarioViewerForm;
    private final ScenarioEditorForm scenarioEditorForm;

    public MainForm() {
        this.frame = new JFrame("Win Deputy");
        this.frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        final Container container = this.frame.getContentPane();
        container.setLayout(new BoxLayout(container, BoxLayout.LINE_AXIS));
        scenarioViewerForm = new ScenarioViewerForm(this);
        scenarioEditorForm = new ScenarioEditorForm(this);
        container.add(scenarioViewerForm);
        container.add(scenarioEditorForm);
        scenarioEditorForm.setVisible(false);
        this.frame.setPreferredSize(new Dimension(500, 500));
    }

    public static void main(String[] args) {
        new MainForm().display();
    }

    public void display() {
        this.frame.pack();
        this.frame.setVisible(true);
    }

    @Override
    public void createNewScenario() {
       scenarioViewerForm.conceal();
       scenarioEditorForm.display();
    }

    @Override
    public void cancelScenarioEditing() {
        scenarioViewerForm.display();
        scenarioEditorForm.conceal();
    }

}
