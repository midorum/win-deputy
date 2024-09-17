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
import java.util.function.Supplier;

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
        SwingUtil.putComponentsToVerticalGrid(this, checkTypeComboBox, checkDataPane);
    }

    CheckEditPane(final Check check, final State state) {
        this(state);
        checkTypeComboBox.setSelectedItem(check.getType());
        final List<CheckDataType> availableDataTypes = getAvailableDataTypes(check.getType());
        checkDataList.clear();
        check.getData().forEach((checkDataType, s) -> checkDataList.add(new CheckDataTypePane(checkDataType, s, availableDataTypes)));
        repaintCheckData();
    }

    private JComboBox<CheckType> createCheckTypeComboBox() {
        final JComboBox<CheckType> checkType = new JComboBox<>(CheckType.values());
        checkType.addActionListener((e) -> {
            final CheckType selectedItem = (CheckType) checkType.getSelectedItem();
            if (selectedItem != null) {
                checkDataList.clear();
                checkDataList.add(new CheckDataTypePane(getAvailableDataTypes(selectedItem)));
                repaintCheckData();
            }
        });
        return checkType;
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
        SwingUtil.putComponentsToVerticalGrid(checkDataPane, -1, checkDataList.toArray(Component[]::new));
        revalidate();
    }

    @Override
    public Check get() throws IllegalInputException {
        final CheckType checkType = validateAndGetCheckType();
        return new Check(checkType, validateAndGetCheckData(checkType));
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
        if (!Check.validateCheckData(checkType, data)) {
            checkDataPane.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
            throw new IllegalInputException("Illegal check data");
        }
        checkDataPane.setBorder(null);
        return data;
    }

    private class CheckDataTypePane extends JPanel {

        private final List<CheckDataType> availableDataTypes;
        private final JPanel sourceTypeEditPaneWrapper;
        private final JComboBox<CheckDataType> dataTypeComboBox;
        private Supplier<String> dataValueSupplier;

        public CheckDataTypePane(final List<CheckDataType> availableDataTypes) {
            this.availableDataTypes = availableDataTypes;
            this.sourceTypeEditPaneWrapper = new JPanel();
            this.dataTypeComboBox = createDataTypeComboBox(availableDataTypes);
            SwingUtil.putComponentsToHorizontalGrid(this,
                    createButtonPane(),
                    dataTypeComboBox,
                    sourceTypeEditPaneWrapper);
        }

        public CheckDataTypePane(final CheckDataType checkDataType, final String dataValue, final List<CheckDataType> availableDataTypes) {
            this(availableDataTypes);
            dataTypeComboBox.setSelectedItem(checkDataType);
            fillSourceTypeEditGrid(checkDataType, dataValue);
        }

        public List<CheckDataType> getAvailableDataTypes() {
            return availableDataTypes;
        }

        private JComboBox<CheckDataType> createDataTypeComboBox(final List<CheckDataType> availableDataTypes) {
            final JComboBox<CheckDataType> comboBox = new JComboBox<>(availableDataTypes.toArray(new CheckDataType[]{}));
            comboBox.setSelectedIndex(-1);
            comboBox.addActionListener((e) -> {
                final CheckDataType selectedItem = (CheckDataType) comboBox.getSelectedItem();
                if (selectedItem != null) fillSourceTypeEditGrid(selectedItem, null);
            });
            return comboBox;
        }

        private void fillSourceTypeEditGrid(final CheckDataType checkDataType, final String dataValue) {
            final SourceTypeEditPane sourceTypeEditPane = new SourceTypeEditPane(checkDataType.getSourceTypes(), dataValue, state);
            dataValueSupplier = sourceTypeEditPane;
            sourceTypeEditPaneWrapper.removeAll();
            SwingUtil.putComponentsToHorizontalGrid(sourceTypeEditPaneWrapper, sourceTypeEditPane);
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

        public CheckDataType getCheckDataType() {
            return (CheckDataType) dataTypeComboBox.getSelectedItem();
        }

        public String getCheckDataValue() {
            return dataValueSupplier != null ? dataValueSupplier.get() : null;
        }
    }

}
