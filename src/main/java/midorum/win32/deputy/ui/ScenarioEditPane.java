package midorum.win32.deputy.ui;

import midorum.win32.deputy.model.Scenario;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

class ScenarioEditPane extends JPanel {

    private JTextField titleField;
    private JTextArea descriptionField;
    private JPanel gridPane;
    private List<ActivityWrapperPane> activities;

    ScenarioEditPane(final Scenario scenario) {
        activities = new ArrayList<>();
        activities.addAll(scenario.getActivities().stream().map(ActivityEditPane::new).map(ActivityWrapperPane::new).collect(Collectors.toCollection(ArrayList::new)));

        compose(scenario.getTitle(), scenario.getDescription());
    }

    ScenarioEditPane() {
        final String title = null;
        final String description = null;
        activities = new ArrayList<>();
        activities.add(new ActivityWrapperPane());

        compose(title, description);
    }

    private void compose(final String title, final String description) {
        final JLabel titleLabel = new JLabel("Title");
        titleField = new JTextField(title);
        final JLabel descriptionLabel = new JLabel("Description");
        descriptionField = new JTextArea(description);
        gridPane = new JPanel(new GridBagLayout());
        final JScrollPane scrollPane = new JScrollPane(gridPane);
        scrollPane.setWheelScrollingEnabled(true);
        scrollPane.getVerticalScrollBar().setUnitIncrement(16);
//        scrollPane.getViewport().addChangeListener(e -> System.out.println("Viewport changed: " + e));
        Util.putComponentsToVerticalGrid(this, titleLabel, titleField, descriptionLabel, descriptionField, scrollPane);
        fillActivitiesGrid();
    }

    private void addActivityAbove(final ActivityWrapperPane activityPane) {
        final int currentIndex = activities.indexOf(activityPane);
        activities.add(currentIndex, new ActivityWrapperPane());
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
        Util.putComponentsToVerticalGrid(gridPane, activities.toArray(Component[]::new));
    }

    private class ActivityWrapperPane extends JPanel {

        private final ActivityEditPane activityEditPane;

        private ActivityWrapperPane(final ActivityEditPane activityEditPane) {
            this.activityEditPane = activityEditPane;
            setBorder(BorderFactory.createLineBorder(Color.DARK_GRAY, 2));
            Util.putComponentsToVerticalGrid(this, createButtonsPane(), activityEditPane);
        }

        public ActivityWrapperPane() {
            this(new ActivityEditPane());
        }

        private JPanel createButtonsPane() {
            final JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            panel.setSize(panel.getPreferredSize());
//            panel.setBorder(BorderFactory.createLineBorder(Color.RED));
            panel.add(new JLabel("Activity"));
            panel.add(createMoveUpButton());
            panel.add(createMoveDownButton());
            panel.add(createAddAboveButton());
            panel.add(createDeleteButton());
            return panel;
        }

        private Button createAddAboveButton() {
            final Button btn = new Button("+^");
            btn.addActionListener(e -> addActivityAbove(this));
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

    }

}
