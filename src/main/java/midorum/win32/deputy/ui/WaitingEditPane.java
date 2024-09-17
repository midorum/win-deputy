package midorum.win32.deputy.ui;

import dma.function.SupplierThrowing;
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
    private final IntegerTextField timeoutField;

    public WaitingEditPane(final Waiting waiting, final State state) {
        this.state = state;
        checks = new ArrayList<>();
        final List<Check> waitingChecks = waiting.getChecks();
        if (waitingChecks != null) {
            checks.addAll(waitingChecks.stream()
                    .map(check -> new CheckEditPane(check, state))
                    .map(CheckWrapperPane::new)
                    .collect(Collectors.toCollection(ArrayList::new)));
        } else {
            checks.add(new CheckWrapperPane(state));
        }
        descriptionField = new JTextArea(waiting.getDescription());
        checksPane = new JPanel(new GridBagLayout());
        timeoutField = new IntegerTextField(0, Integer.MAX_VALUE);
        timeoutField.setText("" + waiting.getTimeout());
        final JPanel timeoutPanel = new JPanel();
        SwingUtil.putComponentsToHorizontalGrid(timeoutPanel, new JLabel("Timeout, seconds"), timeoutField);
        SwingUtil.putComponentsToVerticalGrid(this,
                -1,
                descriptionField,
                checksPane,
                timeoutPanel);
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
            JOptionPane.showMessageDialog(this, "You cannot delete last check");
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
                validateAndGetChecks(),
                validateAndGetTimeout());
    }

    private String validateAndGetDescription() throws IllegalInputException {
        final String description = descriptionField.getText();
        if(description == null || description.isBlank()) {
            descriptionField.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
            throw new IllegalInputException("Waiting should have some description");
        }
        descriptionField.setBorder(null);
        return description;
    }

    private List<Check> validateAndGetChecks() throws IllegalInputException {
        final List<Check> checkList = new ArrayList<>();
        for (CheckWrapperPane next : checks) {
            final Check check = next.get();
            if(!Waiting.validateCheckListItem(check)) {
                next.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
                throw new IllegalInputException("Illegal check");
            }
            next.setBorder(null);
            checkList.add(check);
        }
        if(!Waiting.validateChecks(checkList)) {
            checksPane.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
            throw new IllegalInputException("Illegal checks");
        }
        checksPane.setBorder(null);
        return checkList;
    }

    private long validateAndGetTimeout() throws IllegalInputException {
        final String timeoutText = timeoutField.getText();
        if(timeoutText == null || timeoutText.isBlank()) {
            timeoutField.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
            throw new IllegalInputException("Invalid timeout value (should be integer number)");
        }
        timeoutField.setBorder(null);
        try {
            return Long.parseLong(timeoutText);
        } catch (NumberFormatException e) {
            throw new IllegalInputException("Invalid timeout value (should be integer number)", e);
        }
    }

    private class CheckWrapperPane extends JPanel implements SupplierThrowing<Check, IllegalInputException> {

        private final CheckEditPane checkEditPane;

        private CheckWrapperPane(final CheckEditPane checkEditPane) {
            this.checkEditPane = checkEditPane;
            SwingUtil.putComponentsToVerticalGrid(this, -1, createButtonsPane(), checkEditPane);
        }

        public CheckWrapperPane(final State state) {
            this(new CheckEditPane(state));
        }

        private JPanel createButtonsPane() {
            final JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            panel.setSize(panel.getPreferredSize());
            panel.add(new JLabel("Check"));
            panel.add(createMoveUpButton());
            panel.add(createMoveDownButton());
            panel.add(createAddButton());
            panel.add(createDeleteButton());
            return panel;
        }

        private Button createAddButton() {
            final Button btn = new Button("+");
            btn.addActionListener(e -> addCheckBelow(this));
            return btn;
        }

        private Button createDeleteButton() {
            final Button btn = new Button("x");
            btn.addActionListener(e -> deleteCheck(this));
            return btn;
        }

        private Button createMoveUpButton() {
            final Button btn = new Button("^");
            btn.addActionListener(e -> moveCheckUp(this));
            return btn;
        }

        private Button createMoveDownButton() {
            final Button btn = new Button("v");
            btn.addActionListener(e -> moveCheckDown(this));
            return btn;
        }

        @Override
        public Check get() throws IllegalInputException {
            return checkEditPane.get();
        }
    }

}
