package midorum.win32.deputy.ui;

import midorum.win32.deputy.model.Check;
import midorum.win32.deputy.model.CheckDataType;
import midorum.win32.deputy.model.CheckType;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;

class CheckEditPane extends JPanel {

    private final State state;
    private final ArrayList<CheckDataTypePane> checkDataList;
    private final JPanel checkDataPane;
    private final JComboBox<CheckType> checkTypeComboBox;

    CheckEditPane(final State state) {
        this.state = state;
        checkDataList = new ArrayList<>();
//        setBorder(BorderFactory.createLineBorder(Color.CYAN));
        checkTypeComboBox = createCheckTypeComboBox();
        checkTypeComboBox.setSelectedIndex(-1);
        checkDataPane = new JPanel();
        Util.putComponentsToVerticalGrid(this, checkTypeComboBox, checkDataPane);
    }

    CheckEditPane(final Check check, final State state) {
        this(state);
        checkTypeComboBox.setSelectedItem(check.getType());
        check.getData().forEach((checkDataType, s) -> checkDataList.add(new CheckDataTypePane(checkDataType, s, getAvailableDataTypes(check.getType()))));
        checkDataList.forEach(checkDataPane::add);
    }

    private JComboBox<CheckType> createCheckTypeComboBox() {
        final JComboBox<CheckType> checkType = new JComboBox<>(CheckType.values());
        Util.retainComponentSize(checkType);
        checkType.addActionListener((e) -> {
            final CheckType selectedItem = (CheckType) checkType.getSelectedItem();
            if (selectedItem != null) initCheckData(selectedItem);
        });
        return checkType;
    }

    private void initCheckData(final CheckType checkType) {
        checkDataList.clear();
        final List<CheckDataType> availableDataTypes = getAvailableDataTypes(checkType);
        checkDataList.add(new CheckDataTypePane(availableDataTypes));
        repaintCheckData();
    }

    private static List<CheckDataType> getAvailableDataTypes(final CheckType checkType) {
        final List<CheckDataType> availableDataTypes = new ArrayList<>();
        availableDataTypes.addAll(checkType.mandatory());
        availableDataTypes.addAll(checkType.optional());
        return availableDataTypes;
    }

    private void addDataAbove(final CheckDataTypePane checkDataTypePane) {
        final int currentIndex = checkDataList.indexOf(checkDataTypePane);
        checkDataList.add(currentIndex, new CheckDataTypePane(checkDataTypePane.getAvailableDataTypes()));
        repaintCheckData();
    }

    private void addDataBelow(final CheckDataTypePane checkDataTypePane) {
        final int currentIndex = checkDataList.indexOf(checkDataTypePane);
        checkDataList.add(currentIndex + 1, new CheckDataTypePane(checkDataTypePane.getAvailableDataTypes()));
        repaintCheckData();
    }

    private void deleteData(final CheckDataTypePane checkDataTypePane) {
        if (checkDataList.size() == 1) {
            JOptionPane.showMessageDialog(this, "You cannot delete last check data");
            return;
        }
        checkDataList.remove(checkDataTypePane);
        repaintCheckData();

    }

    private void moveDataUp(final CheckDataTypePane checkDataTypePane) {
        final int currentIndex = checkDataList.indexOf(checkDataTypePane);
        if (currentIndex == 0) return;
        checkDataList.remove(checkDataTypePane);
        checkDataList.add(currentIndex - 1, checkDataTypePane);
        repaintCheckData();
    }

    private void moveDataDown(final CheckDataTypePane checkDataTypePane) {
        final int currentIndex = checkDataList.indexOf(checkDataTypePane);
        if (currentIndex == checkDataList.size() - 1) return;
        checkDataList.remove(checkDataTypePane);
        checkDataList.add(currentIndex + 1, checkDataTypePane);
        repaintCheckData();
    }

    private void repaintCheckData() {
        checkDataPane.removeAll();
        Util.putComponentsToVerticalGrid(checkDataPane, checkDataList.toArray(Component[]::new));
        revalidate();
    }

    private class CheckDataTypePane extends JPanel {

        final private List<CheckDataType> availableDataTypes;
        final private JPanel sourceTypeEditPane;

        public CheckDataTypePane(final CheckDataType checkDataType, final String dataValue, final List<CheckDataType> availableDataTypes) {
            this.availableDataTypes = availableDataTypes;
            this.sourceTypeEditPane = new JPanel();
            Util.putComponentsToHorizontalGrid(this, createButtonPane(), createDataPane(checkDataType, dataValue, availableDataTypes));
        }

        public CheckDataTypePane(final List<CheckDataType> availableDataTypes) {
            this(null, null, availableDataTypes);
        }

        public List<CheckDataType> getAvailableDataTypes() {
            return availableDataTypes;
        }

        private JPanel createDataPane(final CheckDataType checkDataType, final String dataValue, final List<CheckDataType> availableDataTypes) {
            final JPanel dataPane = new JPanel();
            Util.putComponentsToHorizontalGrid(dataPane,
                    createDataTypeComboBox(checkDataType, availableDataTypes),
                    sourceTypeEditPane);
            if (checkDataType != null) {
                fillSourceTypeEditGrid(checkDataType, dataValue);
            }
            return dataPane;
        }

        private JComboBox<CheckDataType> createDataTypeComboBox(final CheckDataType checkDataType, final List<CheckDataType> availableDataTypes) {
            final JComboBox<CheckDataType> dataTypeComboBox = new JComboBox<>(availableDataTypes.toArray(new CheckDataType[]{}));
            if (checkDataType != null) {
                dataTypeComboBox.setSelectedItem(checkDataType);
            } else {
                dataTypeComboBox.setSelectedIndex(-1);
            }
            dataTypeComboBox.addActionListener((e) -> {
                final CheckDataType selectedItem = (CheckDataType) dataTypeComboBox.getSelectedItem();
                if (selectedItem != null) repaintSourceTypeEditPane(selectedItem);
            });
            return dataTypeComboBox;
        }

        private void repaintSourceTypeEditPane(final CheckDataType checkDataType) {
            sourceTypeEditPane.removeAll();
            fillSourceTypeEditGrid(checkDataType, null);
            revalidate();
        }

        private void fillSourceTypeEditGrid(final CheckDataType checkDataType, final String dataValue) {
            Util.putComponentsToHorizontalGrid(sourceTypeEditPane, new SourceTypeEditPane(checkDataType.getSourceTypes(), dataValue, state));
        }

        private JPanel createButtonPane() {
            final JPanel panel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
            panel.setSize(panel.getPreferredSize());
            panel.setBorder(BorderFactory.createLineBorder(Color.RED));
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

    }

}
