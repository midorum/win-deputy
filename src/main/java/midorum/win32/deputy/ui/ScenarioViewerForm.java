package midorum.win32.deputy.ui;

import javax.swing.*;
import java.awt.*;
import java.io.File;
import java.io.IOException;

public class ScenarioViewerForm extends JPanel implements Displayable {

    private static final int PANE_MARGIN = 10;
    private final TaskDispatcher taskDispatcher;

    ScenarioViewerForm(final TaskDispatcher taskDispatcher) {
        this.taskDispatcher = taskDispatcher;
        Util.putComponentsToVerticalGrid(this,
                0,
                new JPanel(),
                createButtonPane());
    }

    private JPanel createButtonPane() {
        final JPanel buttonPane = new JPanel();
        buttonPane.setLayout(new BoxLayout(buttonPane, BoxLayout.LINE_AXIS));
        buttonPane.setBorder(BorderFactory.createEmptyBorder(0, PANE_MARGIN, PANE_MARGIN, PANE_MARGIN));
        buttonPane.add(Box.createHorizontalGlue());
        buttonPane.add(createLoadScenarioButton());
//        buttonPane.add(Box.createRigidArea(new Dimension(10, 0)));
        buttonPane.add(createEditScenarioButton());
//        buttonPane.add(Box.createRigidArea(new Dimension(10, 0)));
        buttonPane.add(createCreateScenarioButton());
        buttonPane.add(createRunScenarioButton());
        buttonPane.add(createStopScenarioButton());
        return buttonPane;
    }

    private Button createLoadScenarioButton() {
        final Button btn = new Button("Load scenario");
        btn.addActionListener(e -> {
            // https://stackoverflow.com/questions/27288636/how-to-load-a-file-using-jfilechooser
            JFileChooser fileChooser = new JFileChooser();
            int returnValue = fileChooser.showOpenDialog(null);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                File selectedFile = fileChooser.getSelectedFile();
                System.out.println("selected file: " + selectedFile.getAbsolutePath());
                System.out.println("selected file exists: " + selectedFile.exists());
                System.out.println("selected file name: " + selectedFile.getName());
//                System.out.println("selected file path: " + selectedFile.getPath());
                System.out.println("selected file path: " + selectedFile.getParentFile().getAbsolutePath());
                if(selectedFile.exists()) {
                    try {
                       // opens file via system
                        Desktop.getDesktop().open(selectedFile);//<-- here
                        // opens file navigator with home directory
//                        Desktop.getDesktop().open(new File(System.getProperty("user.home")));
                    } catch (IOException ex) {
                        throw new RuntimeException(ex);
                    }
                }
            }
        });
        return btn;
    }

    private Button createEditScenarioButton() {
        final Button btn = new Button("Edit scenario");
        btn.addActionListener(e -> System.out.println("actionEvent: " + e));
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
