package midorum.win32.deputy.ui;

import dma.function.SupplierThrowing;
import midorum.win32.deputy.i18n.UiElement;
import midorum.win32.deputy.model.Check;
import midorum.win32.deputy.model.IllegalInputException;
import midorum.win32.deputy.model.Waiting;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

class WaitingEditPane extends JPanel implements SupplierThrowing<Waiting, IllegalInputException> {

    private final State state;
    private final JTextArea descriptionField;
    private final List<CheckWrapperPane> checks;
    private final JPanel checksPane;

    public WaitingEditPane(final Waiting waiting, final State state) {
        this.state = state;
        checks = new ArrayList<>();
        final List<Check> waitingChecks = waiting.getChecks();
        if (waitingChecks != null) {
            checks.addAll(waitingChecks.stream()
                    .map(check -> new CheckWrapperPane(check, state))
                    .collect(Collectors.toCollection(ArrayList::new)));
        } else {
            checks.add(new CheckWrapperPane(state));
        }
        descriptionField = new JTextArea(waiting.getDescription());
        checksPane = new JPanel(new GridBagLayout());
        SwingUtil.putComponentsToVerticalGrid(this,
                -1,
                descriptionField,
                checksPane);
        fillChecksGrid();
    }

    public WaitingEditPane(final State state) {
        this(new Waiting(), state);
    }

    private void addCheckAbove(final CheckWrapperPane checkPane) {
        final int currentIndex = checks.indexOf(checkPane);
        checks.add(currentIndex, new CheckWrapperPane(state));
        repaintChecks();
    }

    private void addCheckBelow(final CheckWrapperPane checkPane) {
        final int currentIndex = checks.indexOf(checkPane);
        checks.add(currentIndex + 1, new CheckWrapperPane(state));
        repaintChecks();
    }

    private void deleteCheck(final CheckWrapperPane checkPane) {
        if (checks.size() == 1) {
            JOptionPane.showMessageDialog(this, UiElement.cannotDeleteLastCheck.forUserLocale());
            return;
        }
        checks.remove(checkPane);
        repaintChecks();
    }

    private void moveCheckUp(final CheckWrapperPane checkPane) {
        final int currentIndex = checks.indexOf(checkPane);
        if (currentIndex == 0) return;
        checks.remove(checkPane);
        checks.add(currentIndex - 1, checkPane);
        repaintChecks();
    }

    private void moveCheckDown(final CheckWrapperPane checkPane) {
        final int currentIndex = checks.indexOf(checkPane);
        if (currentIndex == checks.size() - 1) return;
        checks.remove(checkPane);
        checks.add(currentIndex + 1, checkPane);
        repaintChecks();
    }

    private boolean canIgnoreCheck() {
        if (checks.stream().filter(checkWrapperPane -> !checkWrapperPane.isIgnore()).findAny().isEmpty()) {
            JOptionPane.showMessageDialog(this, UiElement.cannotIgnoreLastCheck);
            return false;
        }
        return true;
    }

    private void repaintChecks() {
        checksPane.removeAll();
        fillChecksGrid();
        revalidate();
    }

    private void fillChecksGrid() {
        SwingUtil.putComponentsToVerticalGrid(checksPane, checks.toArray(Component[]::new));
    }

    @Override
    public Waiting get() throws IllegalInputException {
        return new Waiting(validateAndGetDescription(),
                validateAndGetChecks());
    }

    private String validateAndGetDescription() throws IllegalInputException {
        final String description = descriptionField.getText();
        if (description == null || description.isBlank()) {
            descriptionField.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
            throw new IllegalInputException(UiElement.waitingShouldHaveDescription);
        }
        descriptionField.setBorder(null);
        return description;
    }

    private List<Check> validateAndGetChecks() throws IllegalInputException {
        final List<Check> checkList = new ArrayList<>();
        for (CheckWrapperPane next : checks) {
            final Check check = next.get();
            if (!Waiting.validateCheckListItem(check)) {
                next.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
                throw new IllegalInputException(UiElement.illegalCheck);
            }
            next.setBorder(null);
            checkList.add(check);
        }
        if (!Waiting.validateChecks(checkList)) {
            checksPane.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
            throw new IllegalInputException(UiElement.illegalChecks);
        }
        checksPane.setBorder(null);
        return checkList;
    }

    private class CheckWrapperPane extends JPanel implements SupplierThrowing<Check, IllegalInputException> {

        private final CheckEditPane checkEditPane;
        private final JCheckBox ignoreCheckBox;

        private CheckWrapperPane(final Check check, final State state) {
            this.checkEditPane = new CheckEditPane(check, state);
            this.ignoreCheckBox = createIgnoreCheckBox(check.isIgnore());
            SwingUtil.putComponentsToVerticalGrid(this, -1, createButtonsPane(ignoreCheckBox), checkEditPane);
        }

        public CheckWrapperPane(final State state) {
            this.checkEditPane = new CheckEditPane(state);
            this.ignoreCheckBox = createIgnoreCheckBox(false);
            SwingUtil.putComponentsToVerticalGrid(this, -1, createButtonsPane(ignoreCheckBox), checkEditPane);
        }

        private boolean isIgnore() {
            return ignoreCheckBox.isSelected();
        }

        private JPanel createButtonsPane(final Component... components) {
            final JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            panel.setSize(panel.getPreferredSize());
            panel.add(new JLabel(UiElement.checkLabel.forUserLocale()));
            panel.add(createMoveUpButton());
            panel.add(createMoveDownButton());
            panel.add(createAddButton());
            panel.add(createDeleteButton());
            if (components != null) for (final Component c : components) panel.add(c);
            return panel;
        }

        private Button createAddButton() {
            final Button btn = new Button("+");
            btn.addActionListener(_ -> addCheckBelow(this));
            return btn;
        }

        private Button createDeleteButton() {
            final Button btn = new Button("x");
            btn.addActionListener(_ -> deleteCheck(this));
            return btn;
        }

        private Button createMoveUpButton() {
            final Button btn = new Button("^");
            btn.addActionListener(_ -> moveCheckUp(this));
            return btn;
        }

        private Button createMoveDownButton() {
            final Button btn = new Button("v");
            btn.addActionListener(_ -> moveCheckDown(this));
            return btn;
        }

        private JCheckBox createIgnoreCheckBox(final boolean ignore) {
            final JCheckBox checkBox = new JCheckBox(UiElement.ignoreLabel.forUserLocale());
            checkBox.setSelected(ignore);
            checkBox.addActionListener(_ -> {
                if (checkBox.isSelected() && !canIgnoreCheck()) {
                    checkBox.setSelected(false);
                }
            });
            return checkBox;
        }

        @Override
        public Check get() throws IllegalInputException {
            return checkEditPane.get();
        }
    }

}
