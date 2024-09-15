package midorum.win32.deputy.ui;

import dma.function.SupplierThrowing;
import midorum.win32.deputy.model.Check;
import midorum.win32.deputy.model.CheckDataType;
import midorum.win32.deputy.model.CheckType;
import midorum.win32.deputy.model.IllegalInputException;

import javax.swing.*;
import java.awt.*;
import java.util.*;
import java.util.List;

class CheckEditPane extends JPanel implements SupplierThrowing<Check, IllegalInputException> {

    private final State state;
    private final ArrayList<CheckDataTypePane> checkDataList;
    private final JPanel checkDataPane;
    private final JComboBox<CheckType> checkTypeComboBox;

    CheckEditPane(final State state) {
        this.state = state;
        checkDataList = new ArrayList<>();
        checkTypeComboBox = createCheckTypeComboBox();
        checkTypeComboBox.setSelectedIndex(-1);
        checkDataPane = new JPanel();
        Util.putComponentsToVerticalGrid(this, checkTypeComboBox, checkDataPane);
        setBorder(BorderFactory.createLineBorder(Color.CYAN));
    }

    CheckEditPane(final Check check, final State state) {
        this(state);
        checkTypeComboBox.setSelectedItem(check.getType());
        check.getData().forEach((checkDataType, s) -> checkDataList.add(new CheckDataTypePane(checkDataType, s, getAvailableDataTypes(check.getType()))));
        checkDataList.forEach(checkDataPane::add);
    }

    private JComboBox<CheckType> createCheckTypeComboBox() {
        final JComboBox<CheckType> checkType = new JComboBox<>(CheckType.values());
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
        Util.putComponentsToVerticalGrid(checkDataPane, -1, checkDataList.toArray(Component[]::new));
        revalidate();
    }

    @Override
    public Check get() throws IllegalInputException {
        final CheckType checkType = validateAndGetCheckType();
        final Map<CheckDataType, String> data = validateAndGetCheckData(checkType);
        return new Check(checkType, data);
    }

    private CheckType validateAndGetCheckType() throws IllegalInputException {
        final CheckType checkType = (CheckType) checkTypeComboBox.getSelectedItem();
        if (!Check.validateCheckType(checkType)) {
            checkTypeComboBox.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
            throw new IllegalInputException("Illegal check type");
        }
        checkTypeComboBox.setBorder(null);
        return checkType;
    }

    private Map<CheckDataType, String> validateAndGetCheckData(final CheckType checkType) throws IllegalInputException {
        final Map<CheckDataType, String> data = new HashMap<>(checkDataList.size());
        for (final CheckDataTypePane next : checkDataList) {
            final CheckDataType checkDataType = next.getCheckDataType();
            final String checkDataValue = next.getCheckDataValue();
            if (!Check.validateCheckDataEntry(checkDataType, checkDataValue)) {
                next.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
                throw new IllegalInputException("Illegal check data");
            }
            next.setBorder(null);
            data.put(checkDataType, checkDataValue);
        }
        if(!Check.validateCheckData(checkType, data)){
            checkDataPane.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
            throw new IllegalInputException("Illegal check data");
        }
        checkDataPane.setBorder(null);
        return data;
    }

    private class CheckDataTypePane extends JPanel {

        final private List<CheckDataType> availableDataTypes;
        final private JPanel sourceTypeEditPaneWrapper;
        private JComboBox<CheckDataType> dataTypeComboBox;
        private SourceTypeEditPane sourceTypeEditPane;

        public CheckDataTypePane(final CheckDataType checkDataType, final String dataValue, final List<CheckDataType> availableDataTypes) {
            this.availableDataTypes = availableDataTypes;
            this.sourceTypeEditPaneWrapper = new JPanel();
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
            dataTypeComboBox = createDataTypeComboBox(checkDataType, availableDataTypes);
            Util.putComponentsToHorizontalGrid(dataPane,
                    dataTypeComboBox,
                    sourceTypeEditPaneWrapper);
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
            sourceTypeEditPaneWrapper.removeAll();
            fillSourceTypeEditGrid(checkDataType, null);
            revalidate();
        }

        private void fillSourceTypeEditGrid(final CheckDataType checkDataType, final String dataValue) {
            sourceTypeEditPane = new SourceTypeEditPane(checkDataType.getSourceTypes(), dataValue, state);
            Util.putComponentsToHorizontalGrid(sourceTypeEditPaneWrapper, sourceTypeEditPane);
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

        public CheckDataType getCheckDataType() {
            return (CheckDataType) dataTypeComboBox.getSelectedItem();
        }

        public String getCheckDataValue() {
            return sourceTypeEditPane != null ? sourceTypeEditPane.get() : null;
        }
    }

}
