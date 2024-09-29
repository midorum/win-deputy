package midorum.win32.deputy.ui;

import dma.function.SupplierThrowing;
import midorum.win32.deputy.common.CommonUtil;
import midorum.win32.deputy.model.*;

import javax.swing.*;
import java.awt.*;
import java.util.*;
import java.util.List;
import java.util.function.Supplier;
import java.util.stream.Collectors;

class ScenarioEditPane extends JPanel implements SupplierThrowing<Scenario, IllegalInputException> {

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
                .map(activity -> new ActivityWrapperPane(activity, state))
                .collect(Collectors.toCollection(ArrayList::new)));
        compose(scenario.getTitle(), scenario.getDescription().orElse(null), scenario.getType(), scenario.getData().orElse(null));
    }

    ScenarioEditPane(final State state) {
        this.state = state;
        activities = new ArrayList<>();
        activities.add(new ActivityWrapperPane(state));
        compose(null, null, null, null);
    }

    private void compose(final String title, final String description, final ScenarioType scenarioType, final Map<ScenarioDataType, String> data) {
        titleField = new JTextField(title);
        descriptionField = new JTextArea(description);
        gridPane = new JPanel();
        final JScrollPane scrollPane = new JScrollPane(gridPane);
        scrollPane.setWheelScrollingEnabled(true);
        scrollPane.getVerticalScrollBar().setUnitIncrement(16);
//        scrollPane.getViewport().addChangeListener(e -> System.out.println("Viewport changed: " + e));
        scenarioTypeEditPane = new ScenarioTypeEditPane(scenarioType, data, state);
        SwingUtil.putComponentsToVerticalGrid(this,
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

    private void addActivityCopyBelow(final ActivityWrapperPane activityPane, final Activity activity) {
        final int currentIndex = activities.indexOf(activityPane);
        activities.add(currentIndex + 1, new ActivityWrapperPane(activity, state));
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

    private boolean canIgnoreActivity() {
        if (activities.stream().filter(checkWrapperPane -> !checkWrapperPane.isIgnore()).findAny().isEmpty()) {
            JOptionPane.showMessageDialog(this, "You cannot ignore last activity from scenario");
            return false;
        }
        return true;
    }

    private void repaintActivities() {
        gridPane.removeAll();
        fillActivitiesGrid();
        revalidate();
    }

    private void fillActivitiesGrid() {
        SwingUtil.putComponentsToVerticalGrid(gridPane, -1, activities.toArray(Component[]::new));
    }

    @Override
    public Scenario get() throws IllegalInputException {
        return new Scenario(validateAndGetScenarioTitle(),
                descriptionField.getText(),
                validateAndGetActivities(),
                validateAndGetScenarioType(),
                validateAndGetScenarioData());
    }

    private String validateAndGetScenarioTitle() throws IllegalInputException {
        final String scenarioTitle = titleField.getText();
        if (scenarioTitle == null || scenarioTitle.isBlank()) {
            titleField.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
            throw new IllegalInputException("Scenario title cannot be empty");
        }
        titleField.setBorder(null);
        return scenarioTitle;
    }

    private List<Activity> validateAndGetActivities() throws IllegalInputException {
        final List<Activity> activitiesList = new ArrayList<>();
        for (ActivityWrapperPane next : activities) {
            final Activity activity = next.get();
            if (!Scenario.validateActivityListItem(activity)) {
                next.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
                throw new IllegalInputException("Illegal activity");
            }
            next.setBorder(BorderFactory.createLineBorder(Color.DARK_GRAY, 2));
            activitiesList.add(activity);
        }
        if (!Scenario.validateActivities(activitiesList)) {
            gridPane.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
            throw new IllegalInputException("Illegal activities");
        }
        gridPane.setBorder(null);
        return activitiesList;
    }

    private ScenarioType validateAndGetScenarioType() throws IllegalInputException {
        final ScenarioType scenarioType = scenarioTypeEditPane.getScenarioType();
        if (scenarioType == null) {
            scenarioTypeEditPane.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
            throw new IllegalInputException("Scenario type cannot be empty");
        }
        scenarioTypeEditPane.setBorder(null);
        return scenarioType;
    }

    private Map<ScenarioDataType, String> validateAndGetScenarioData() throws IllegalInputException {
        return scenarioTypeEditPane.validateAndGetScenarioData();
    }

    private class ActivityWrapperPane extends JPanel implements SupplierThrowing<Activity, IllegalInputException> {

        private final State state;
        private SupplierThrowing<Activity, IllegalInputException> activitySupplier;
        private final JCheckBox ignoreCheckBox;

        private ActivityWrapperPane(final Activity activity, final State state) {
            this.state = state;
            this.ignoreCheckBox = createIgnoreCheckBox(activity.isIgnore());
            setBorder(BorderFactory.createLineBorder(Color.DARK_GRAY, 2));
            fillActivityContainer(activity);
        }

        public ActivityWrapperPane(final State state) {
            this(new Activity(), state);
        }

        private boolean isIgnore() {
            return ignoreCheckBox.isSelected();
        }

        private void fillActivityContainer(final Activity activity, final boolean forEditing) {
            removeAll();
            if (!forEditing && activity.getTitle() != null) {
                final ActivityViewPane activityViewPane = new ActivityViewPane(activity);
                this.activitySupplier = activityViewPane;
                this.ignoreCheckBox.setEnabled(false);
                SwingUtil.putComponentsToVerticalGrid(this, createActivityViewButtonsPane(ignoreCheckBox), activityViewPane);
            } else {
                final ActivityEditPane activityEditPane = new ActivityEditPane(activity, state);
                this.activitySupplier = activityEditPane;
                this.ignoreCheckBox.setEnabled(true);
                SwingUtil.putComponentsToVerticalGrid(this, createActivityEditButtonsPane(ignoreCheckBox), activityEditPane);
            }
            revalidate();
        }

        private void fillActivityContainer(final Activity activity) {
            fillActivityContainer(activity, false);
        }

        private JPanel createActivityViewButtonsPane(final Component... components) {
            final JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            panel.setSize(panel.getPreferredSize());
            panel.add(new JLabel("Activity viewing"));
            panel.add(createEditActivityButton());
            panel.add(createAddButton());
            if (components != null) for (final Component c : components) panel.add(c);
            return panel;
        }

        private JPanel createActivityEditButtonsPane(final Component... components) {
            final JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            panel.setSize(panel.getPreferredSize());
            panel.add(new JLabel("Activity editing"));
            panel.add(createViewActivityButton());
            panel.add(createMoveUpButton());
            panel.add(createMoveDownButton());
            panel.add(createAddButton());
            panel.add(createAddCopyButton());
            panel.add(createDeleteButton());
            if (components != null) for (final Component c : components) panel.add(c);
            return panel;
        }

        private Button createAddButton() {
            final Button btn = new Button("+");
            btn.addActionListener(e -> {
                try {
                    final Activity activity = activitySupplier.get();
                    fillActivityContainer(activity);
                    addActivityBelow(this);
                } catch (IllegalInputException ex) {
                    state.getUtilities().reportIllegalState("You should fulfill this activity properly before adding a new one");
                }
            });
            return btn;
        }

        private Button createAddCopyButton() {
            final Button btn = new Button("++");
            btn.addActionListener(e -> {
                try {
                    final Activity activity = activitySupplier.get();
                    fillActivityContainer(activity);
                    addActivityCopyBelow(this, activity);
                } catch (IllegalInputException ex) {
                    state.getUtilities().reportIllegalState("You should fulfill this activity properly before adding a new one");
                }
            });
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

        private Button createEditActivityButton() {
            final Button btn = new Button("E");
            btn.addActionListener(e -> {
                try {
                    final Activity activity = activitySupplier.get();
                    fillActivityContainer(activity, true);
                } catch (IllegalInputException ex) {
                    state.getUtilities().reportIllegalState("You should fulfill this activity properly before editing");
                }
            });
            return btn;
        }

        private Component createViewActivityButton() {
            final Button btn = new Button("A");
            btn.addActionListener(e -> {
                try {
                    final Activity activity = activitySupplier.get();
                    fillActivityContainer(activity, false);
                } catch (IllegalInputException ex) {
                    state.getUtilities().reportIllegalState("You should fulfill this activity properly before viewing");
                }
            });
            return btn;
        }

        private JCheckBox createIgnoreCheckBox(final boolean ignore) {
            final JCheckBox checkBox;
            checkBox = new JCheckBox("ignore");
            checkBox.setSelected(ignore);
            checkBox.addActionListener(e -> {
                if(checkBox.isSelected() && !canIgnoreActivity()) {
                    checkBox.setSelected(false);
                }
            });
            return checkBox;
        }

        @Override
        public Activity get() throws IllegalInputException {
            final Activity activity = activitySupplier.get();
            return new Activity(activity.getTitle(),
                    activity.getDescription().orElse(null),
                    activity.getChecks(),
                    activity.getCommands(),
                    activity.getWaitingList().orElse(null),
                    activity.isRepeatable(),
                    activity.producesFragileState(),
                    this.ignoreCheckBox.isSelected());
        }

    }

    private class ScenarioTypeEditPane extends JPanel {

        private final JComboBox<ScenarioType> checkTypeComboBox;
        private final ArrayList<ScenarioDataTypePane> scenarioDataTypeList;
        private final JPanel scenarioDataTypePane;

        public ScenarioTypeEditPane(final ScenarioType scenarioType, final Map<ScenarioDataType, String> data, final State state) {
            this.checkTypeComboBox = createScenarioTypeComboBox();
            this.scenarioDataTypeList = new ArrayList<>();
            this.scenarioDataTypePane = new JPanel();
            SwingUtil.putComponentsToVerticalGrid(this, checkTypeComboBox, scenarioDataTypePane);
            if (scenarioType != null) {
                checkTypeComboBox.setSelectedItem(scenarioType);
                final List<ScenarioDataType> availableDataTypes = getAvailableDataTypes(scenarioType);
                if (data != null) {
                    scenarioDataTypeList.clear();
                    data.forEach((scenarioDataType, s) -> scenarioDataTypeList.add(new ScenarioDataTypePane(scenarioDataType, s, availableDataTypes)));
                    repaintScenarioData();
                }
            }
        }

        private JComboBox<ScenarioType> createScenarioTypeComboBox() {
            final JComboBox<ScenarioType> comboBox = new JComboBox<>(ScenarioType.values());
            comboBox.setSelectedIndex(-1);
            comboBox.addActionListener((e) -> {
                final ScenarioType selectedItem = (ScenarioType) comboBox.getSelectedItem();
                if (selectedItem != null) {
                    scenarioDataTypeList.clear();
                    if (!selectedItem.mandatory().isEmpty() || !selectedItem.optional().isEmpty()) {
                        scenarioDataTypeList.add(new ScenarioDataTypePane(getAvailableDataTypes(selectedItem)));
                    }
                    repaintScenarioData();
                }
            });
            return comboBox;
        }

        private List<ScenarioDataType> getAvailableDataTypes(final ScenarioType scenarioType) {
            return CommonUtil.concatLists(scenarioType.mandatory(), scenarioType.optional());
        }

        private void addDataAbove(final ScenarioDataTypePane scenarioDataTypePane) {
            final int currentIndex = scenarioDataTypeList.indexOf(scenarioDataTypePane);
            scenarioDataTypeList.add(currentIndex, new ScenarioDataTypePane(scenarioDataTypePane.getAvailableDataTypes()));
            repaintScenarioData();
        }

        private void addDataBelow(final ScenarioDataTypePane scenarioDataTypePane) {
            final int currentIndex = scenarioDataTypeList.indexOf(scenarioDataTypePane);
            scenarioDataTypeList.add(currentIndex + 1, new ScenarioDataTypePane(scenarioDataTypePane.getAvailableDataTypes()));
            repaintScenarioData();
        }

        private void deleteData(final ScenarioDataTypePane scenarioDataTypePane) {
            if (scenarioDataTypeList.size() == 1) {
                JOptionPane.showMessageDialog(this, "You cannot delete last scenario data");
                return;
            }
            scenarioDataTypeList.remove(scenarioDataTypePane);
            repaintScenarioData();
        }

        private void moveDataUp(final ScenarioDataTypePane scenarioDataTypePane) {
            final int currentIndex = scenarioDataTypeList.indexOf(scenarioDataTypePane);
            if (currentIndex == 0) return;
            scenarioDataTypeList.remove(scenarioDataTypePane);
            scenarioDataTypeList.add(currentIndex - 1, scenarioDataTypePane);
            repaintScenarioData();
        }

        private void moveDataDown(final ScenarioDataTypePane scenarioDataTypePane) {
            final int currentIndex = scenarioDataTypeList.indexOf(scenarioDataTypePane);
            if (currentIndex == scenarioDataTypeList.size() - 1) return;
            scenarioDataTypeList.remove(scenarioDataTypePane);
            scenarioDataTypeList.add(currentIndex + 1, scenarioDataTypePane);
            repaintScenarioData();
        }

        private void repaintScenarioData() {
            scenarioDataTypePane.removeAll();
            SwingUtil.putComponentsToVerticalGrid(scenarioDataTypePane, -1, scenarioDataTypeList.toArray(Component[]::new));
            revalidate();
        }

        public ScenarioType getScenarioType() {
            return (ScenarioType) checkTypeComboBox.getSelectedItem();
        }

        public Map<ScenarioDataType, String> validateAndGetScenarioData() throws IllegalInputException {
            final Map<ScenarioDataType, String> data = new HashMap<>(scenarioDataTypeList.size());
            for (final ScenarioDataTypePane next : scenarioDataTypeList) {
                final ScenarioDataType scenarioDataType = next.getScenarioDataType();
                final String scenarioDataValue = next.getScenarioDataValue();
                if (!Scenario.validateScenarioDataEntry(scenarioDataType, scenarioDataValue)) {
                    next.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
                    throw new IllegalInputException("Illegal scenario data");
                }
                next.setBorder(null);
                data.put(scenarioDataType, scenarioDataValue);
            }
            if (!Scenario.validateScenarioData(getScenarioType(), data)) {
                scenarioDataTypePane.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
                throw new IllegalInputException("Illegal scenario data");
            }
            scenarioDataTypePane.setBorder(null);
            return data;
        }

        private class ScenarioDataTypePane extends JPanel {

            private final List<ScenarioDataType> availableDataTypes;
            private final JPanel sourceTypeEditPaneWrapper;
            private final JComboBox<ScenarioDataType> dataTypeComboBox;
            private Supplier<String> dataValueSupplier;

            public ScenarioDataTypePane(final List<ScenarioDataType> availableDataTypes) {
                this.availableDataTypes = availableDataTypes;
                this.sourceTypeEditPaneWrapper = new JPanel();
                this.dataTypeComboBox = createDataTypeComboBox(availableDataTypes);
                SwingUtil.putComponentsToHorizontalGrid(this,
                        createButtonPane(),
                        dataTypeComboBox,
                        sourceTypeEditPaneWrapper);
            }

            public ScenarioDataTypePane(final ScenarioDataType scenarioDataType, final String dataValue, final List<ScenarioDataType> availableDataTypes) {
                this(availableDataTypes);
                dataTypeComboBox.setSelectedItem(scenarioDataType);
                fillSourceTypeEditGrid(scenarioDataType, dataValue);
            }

            public List<ScenarioDataType> getAvailableDataTypes() {
                return this.availableDataTypes;
            }

            private JComboBox<ScenarioDataType> createDataTypeComboBox(final List<ScenarioDataType> availableDataTypes) {
                final JComboBox<ScenarioDataType> comboBox = new JComboBox<>(availableDataTypes.toArray(new ScenarioDataType[]{}));
                comboBox.setSelectedIndex(-1);
                comboBox.addActionListener((e) -> {
                    final ScenarioDataType selectedItem = (ScenarioDataType) comboBox.getSelectedItem();
                    if (selectedItem != null) fillSourceTypeEditGrid(selectedItem, null);
                });
                return comboBox;
            }

            private void fillSourceTypeEditGrid(final ScenarioDataType scenarioDataType, final String dataValue) {
                final SourceTypeEditPane sourceTypeEditPane = new SourceTypeEditPane(scenarioDataType.getSourceTypes(), dataValue, state);
                dataValueSupplier = sourceTypeEditPane;
                sourceTypeEditPaneWrapper.removeAll();
                SwingUtil.putComponentsToHorizontalGrid(sourceTypeEditPaneWrapper,
                        0,
                        sourceTypeEditPane,
                        new JLabel(scenarioDataType.getUnit().map(timeUnit -> timeUnit.toString().toLowerCase())
                                .orElse(null)));
                sourceTypeEditPaneWrapper.revalidate();
            }

            private JPanel createButtonPane() {
                final JPanel panel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
                panel.setSize(panel.getPreferredSize());
                panel.add(createMoveUpButton());
                panel.add(createMoveDownButton());
                panel.add(createAddButton());
                panel.add(createDeleteButton());
                return panel;
            }

            private Button createAddButton() {
                final Button btn = new Button("+");
                btn.addActionListener(e -> addDataBelow(this));
                return btn;
            }

            private Button createDeleteButton() {
                final Button btn = new Button("x");
                btn.addActionListener(e -> deleteData(this));
                return btn;
            }

            private Button createMoveUpButton() {
                final Button btn = new Button("^");
                btn.addActionListener(e -> moveDataUp(this));
                return btn;
            }

            private Button createMoveDownButton() {
                final Button btn = new Button("v");
                btn.addActionListener(e -> moveDataDown(this));
                return btn;
            }

            public ScenarioDataType getScenarioDataType() {
                return (ScenarioDataType) dataTypeComboBox.getSelectedItem();
            }

            public String getScenarioDataValue() {
                return dataValueSupplier != null ? dataValueSupplier.get() : null;
            }

        }
    }
}
