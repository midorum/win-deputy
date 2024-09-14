package midorum.win32.deputy.ui;

import midorum.win32.deputy.model.Activity;
import midorum.win32.deputy.model.Scenario;
import midorum.win32.deputy.model.ScenarioType;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;
import java.util.stream.Collectors;

class ScenarioEditPane extends JPanel implements Supplier<Scenario> {

    private final State state;
    private JTextField titleField;
    private JTextArea descriptionField;
    private JPanel gridPane;
    private final List<ActivityWrapperPane> activities;
    private ScenarioTypeEditPane scenarioTypeEditPane;

    ScenarioEditPane(final Scenario scenario, final State state) {
        this.state = state;
        activities = new ArrayList<>();
        activities.addAll(scenario.getActivities().stream()
                .map(activity -> new ActivityEditPane(activity, state))
                .map(ActivityWrapperPane::new)
                .collect(Collectors.toCollection(ArrayList::new)));

        compose(scenario.getTitle(), scenario.getDescription(), scenario.getType());
    }

    ScenarioEditPane(final State state) {
        this.state = state;
        activities = new ArrayList<>();
        activities.add(new ActivityWrapperPane(state));

        compose(null, null, null);
    }

    private void compose(final String title, final String description, final ScenarioType scenarioType) {
        titleField = new JTextField(title);
        descriptionField = new JTextArea(description);
        gridPane = new JPanel();
        final JScrollPane scrollPane = new JScrollPane(gridPane);
        scrollPane.setWheelScrollingEnabled(true);
        scrollPane.getVerticalScrollBar().setUnitIncrement(16);
//        scrollPane.getViewport().addChangeListener(e -> System.out.println("Viewport changed: " + e));
        scenarioTypeEditPane = new ScenarioTypeEditPane(scenarioType);
        Util.putComponentsToVerticalGrid(this,
                new double[]{0.0, 0.0, 0.0, 0.0, 0.5, 0.0},
                new JLabel("Title"),
                titleField,
                new JLabel("Description"),
                descriptionField,
                scrollPane,
                scenarioTypeEditPane);
        fillActivitiesGrid();
    }

    private void addActivityAbove(final ActivityWrapperPane activityPane) {
        final int currentIndex = activities.indexOf(activityPane);
        activities.add(currentIndex, new ActivityWrapperPane(state));
        repaintActivities();
    }

    private void addActivityBelow(final ActivityWrapperPane activityPane) {
        final int currentIndex = activities.indexOf(activityPane);
        activities.add(currentIndex + 1, new ActivityWrapperPane(state));
        repaintActivities();
    }

    private void deleteActivity(final ActivityWrapperPane activityPane) {
        if (activities.size() == 1) {
            JOptionPane.showMessageDialog(this, "You cannot delete last activity from scenario");
            return;
        }
        activities.remove(activityPane);
        repaintActivities();
    }

    private void moveActivityUp(final ActivityWrapperPane activityPane) {
        final int currentIndex = activities.indexOf(activityPane);
        if (currentIndex == 0) return;
        activities.remove(activityPane);
        activities.add(currentIndex - 1, activityPane);
        repaintActivities();
    }

    private void moveActivityDown(final ActivityWrapperPane activityPane) {
        final int currentIndex = activities.indexOf(activityPane);
        if (currentIndex == activities.size() - 1) return;
        activities.remove(activityPane);
        activities.add(currentIndex + 1, activityPane);
        repaintActivities();
    }

    private void repaintActivities() {
        gridPane.removeAll();
        fillActivitiesGrid();
        revalidate();
    }

    private void fillActivitiesGrid() {
        Util.putComponentsToVerticalGrid(gridPane, -1, activities.toArray(Component[]::new));
    }

    @Override
    public Scenario get() {
        return new Scenario(titleField.getText(),
                descriptionField.getText(),
                activities.stream().map(ActivityWrapperPane::get).toList(),
                scenarioTypeEditPane.getScenarioType(), null);
    }

    private class ActivityWrapperPane extends JPanel implements Supplier<Activity> {

        private final ActivityEditPane activityEditPane;

        private ActivityWrapperPane(final ActivityEditPane activityEditPane) {
            this.activityEditPane = activityEditPane;
            setBorder(BorderFactory.createLineBorder(Color.DARK_GRAY, 2));
            Util.putComponentsToVerticalGrid(this, createButtonsPane(), activityEditPane);
        }

        public ActivityWrapperPane(final State state) {
            this(new ActivityEditPane(state));
        }

        private JPanel createButtonsPane() {
            final JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            panel.setSize(panel.getPreferredSize());
//            panel.setBorder(BorderFactory.createLineBorder(Color.RED));
            panel.add(new JLabel("Activity"));
            panel.add(createMoveUpButton());
            panel.add(createMoveDownButton());
            panel.add(createAddButton());
            panel.add(createDeleteButton());
            return panel;
        }

        private Button createAddButton() {
            final Button btn = new Button("+");
            btn.addActionListener(e -> addActivityBelow(this));
            return btn;
        }

        private Button createDeleteButton() {
            final Button btn = new Button("x");
            btn.addActionListener(e -> deleteActivity(this));
            return btn;
        }

        private Button createMoveUpButton() {
            final Button btn = new Button("^");
            btn.addActionListener(e -> moveActivityUp(this));
            return btn;
        }

        private Button createMoveDownButton() {
            final Button btn = new Button("v");
            btn.addActionListener(e -> moveActivityDown(this));
            return btn;
        }

        @Override
        public Activity get() {
            return activityEditPane.get();
        }
    }

    private class ScenarioTypeEditPane extends JPanel {

        private final JComboBox<ScenarioType> checkTypeComboBox;

        public ScenarioTypeEditPane(final ScenarioType scenarioType) {
            this.checkTypeComboBox = new JComboBox<>(ScenarioType.values());
            if (scenarioType != null)
                checkTypeComboBox.setSelectedItem(scenarioType);
            else
                checkTypeComboBox.setSelectedIndex(-1);
            Util.putComponentsToVerticalGrid(this, checkTypeComboBox);
        }

        public ScenarioType getScenarioType() {
            return (ScenarioType) checkTypeComboBox.getSelectedItem();
        }

    }
}
