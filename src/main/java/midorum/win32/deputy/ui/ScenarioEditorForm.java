package midorum.win32.deputy.ui;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import midorum.win32.deputy.model.Scenario;

import javax.swing.*;
import java.awt.*;
import java.util.function.Supplier;

class ScenarioEditorForm extends JPanel implements Displayable {

    private static final int PANE_MARGIN = 10;
    private final TaskDispatcher taskDispatcher;
    private final State state;
    private final Supplier<Scenario> scenarioSupplier;

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
            final Scenario scenario = scenarioSupplier.get();
            System.out.println(scenario);
            final ObjectMapper objectMapper = new ObjectMapper();
            objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
            try {
                final String s = objectMapper.writeValueAsString(scenario);
                System.out.println(s);
                final Scenario parsed = objectMapper.readValue(s, Scenario.class);
                System.out.println(parsed);
                System.out.println("-----------------");
            } catch (JsonProcessingException ex) {
                throw new RuntimeException(ex);
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
