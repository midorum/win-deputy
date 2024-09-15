package midorum.win32.deputy.ui;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.module.paramnames.ParameterNamesModule;
import dma.function.SupplierThrowing;
import midorum.win32.deputy.model.IllegalInputException;
import midorum.win32.deputy.model.Scenario;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.awt.*;
import java.io.File;
import java.io.IOException;

class ScenarioEditorForm extends JPanel implements Displayable {

    private static final int PANE_MARGIN = 10;
    private final TaskDispatcher taskDispatcher;
    private final SupplierThrowing<Scenario, IllegalInputException> scenarioSupplier;
    private final Logger logger = LogManager.getLogger(this);
    private final State state;

    ScenarioEditorForm(final TaskDispatcher taskDispatcher) {
        this.taskDispatcher = taskDispatcher;
        this.state = new State();
        final ScenarioEditPane scenarioEditPane = new ScenarioEditPane(state);
        this.scenarioSupplier = scenarioEditPane;
        scenarioEditPane.setBorder(BorderFactory.createEmptyBorder(0, PANE_MARGIN, PANE_MARGIN, PANE_MARGIN));
        Util.putComponentsToVerticalGrid(this,
                1,
                new JLabel("Scenario path will be here"),
                scenarioEditPane,
                createButtonPane());
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
        btn.addActionListener(e -> {
            // ~~~
            try {
                final Scenario scenario = scenarioSupplier.get();
                System.out.println(scenario);
                Util.askNewFile(state.getWorkingDirectory(), this)
                        .ifPresentOrElse(file -> {
                            final ObjectMapper objectMapper = new ObjectMapper()
                                    .registerModule(new ParameterNamesModule())
                                    .registerModule(new Jdk8Module())
                                    .registerModule(new JavaTimeModule());
                            objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
                            try {
                                final File scenarioFile = new File(file.getAbsolutePath() + ".wds");
                                objectMapper.writeValue(scenarioFile, scenario);
                                final Scenario parsed = objectMapper.readValue(scenarioFile, Scenario.class);
                                System.out.println(parsed);
                            } catch (IOException ex) {
                                logger.error(ex.getMessage(), ex);
                                JOptionPane.showMessageDialog(this, ex.getMessage());
                            }

                        }, () -> {
                        });
            } catch (IllegalInputException ex) {
                logger.error(ex.getMessage(), ex);
                JOptionPane.showMessageDialog(this, ex.getMessage());
            }
        });
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
