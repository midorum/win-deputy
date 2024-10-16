package midorum.win32.deputy.ui;

import dma.function.SupplierThrowing;
import midorum.win32.deputy.i18n.UiElement;
import midorum.win32.deputy.model.*;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

class WaitingListEditPane extends JPanel implements SupplierThrowing<WaitingList, IllegalInputException> {

    private final State state;
    private final List<WaitingWrapperPane> wrapperPaneList;
    private final JPanel waitingListPane;
    private final IntegerTextField timeoutField;

    WaitingListEditPane(final WaitingList waitingList, final State state) {
        this.state = state;
        wrapperPaneList = new ArrayList<>();
        final List<Waiting> list = waitingList.getList();
        if (list != null) {
            wrapperPaneList.addAll(list.stream()
                    .map(waiting -> new WaitingEditPane(waiting, state))
                    .map(WaitingWrapperPane::new)
                    .collect(Collectors.toCollection(ArrayList::new)));
        } else {
            wrapperPaneList.add(new WaitingWrapperPane(state));
        }
        waitingListPane = new JPanel(new GridBagLayout());
        waitingListPane.setBorder(BorderFactory.createLineBorder(Color.GREEN, 2));
        timeoutField = new IntegerTextField(0, Integer.MAX_VALUE);
        timeoutField.setText("" + waitingList.getTimeout());
        final JPanel timeoutPanel = new JPanel();
        SwingUtil.putComponentsToHorizontalGrid(timeoutPanel,
                new JLabel(UiElement.timeoutInSecondsLabel.forUserLocale()), timeoutField);
        SwingUtil.putComponentsToVerticalGrid(this,
                -1,
                waitingListPane,
                timeoutPanel);
        fillWaitingGrid();
    }

    private void addCheckAbove(final WaitingWrapperPane checkPane) {
        final int currentIndex = wrapperPaneList.indexOf(checkPane);
        wrapperPaneList.add(currentIndex, new WaitingWrapperPane(state));
        repaintWrapperPaneList();
    }

    private void addCheckBelow(final WaitingWrapperPane checkPane) {
        final int currentIndex = wrapperPaneList.indexOf(checkPane);
        wrapperPaneList.add(currentIndex + 1, new WaitingWrapperPane(state));
        repaintWrapperPaneList();
    }

    private void deleteCheck(final WaitingWrapperPane checkPane) {
        if (wrapperPaneList.size() == 1) {
            JOptionPane.showMessageDialog(this, UiElement.cannotDeleteLastCheck);
            return;
        }
        wrapperPaneList.remove(checkPane);
        repaintWrapperPaneList();
    }

    private void moveCheckUp(final WaitingWrapperPane checkPane) {
        final int currentIndex = wrapperPaneList.indexOf(checkPane);
        if (currentIndex == 0) return;
        wrapperPaneList.remove(checkPane);
        wrapperPaneList.add(currentIndex - 1, checkPane);
        repaintWrapperPaneList();
    }

    private void moveCheckDown(final WaitingWrapperPane checkPane) {
        final int currentIndex = wrapperPaneList.indexOf(checkPane);
        if (currentIndex == wrapperPaneList.size() - 1) return;
        wrapperPaneList.remove(checkPane);
        wrapperPaneList.add(currentIndex + 1, checkPane);
        repaintWrapperPaneList();
    }

    private void repaintWrapperPaneList() {
        waitingListPane.removeAll();
        fillWaitingGrid();
        revalidate();
    }

    private void fillWaitingGrid() {
        SwingUtil.putComponentsToVerticalGrid(waitingListPane, wrapperPaneList.toArray(Component[]::new));
    }

    @Override
    public WaitingList get() throws IllegalInputException {
        return new WaitingList(
                validateAndGetChecks(),
                validateAndGetTimeout());
    }

    private List<Waiting> validateAndGetChecks() throws IllegalInputException {
        final List<Waiting> waitingList = new ArrayList<>();
        for (WaitingWrapperPane next : wrapperPaneList) {
            final Waiting waiting = next.get();
            if (!WaitingList.validateListItem(waiting)) {
                next.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
                throw new IllegalInputException(UiElement.illegalWaiting);
            }
            next.setBorder(null);
            waitingList.add(waiting);
        }
        if (!WaitingList.validateList(waitingList)) {
            waitingListPane.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
            throw new IllegalInputException(UiElement.illegalWaitingList);
        }
        waitingListPane.setBorder(null);
        return waitingList;
    }

    private long validateAndGetTimeout() throws IllegalInputException {
        final String timeoutText = timeoutField.getText();
        if (timeoutText == null || timeoutText.isBlank()) {
            timeoutField.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
            throw new IllegalInputException(UiElement.illegalTimeoutValue);
        }
        timeoutField.setBorder(null);
        try {
            return Long.parseLong(timeoutText);
        } catch (NumberFormatException e) {
            throw new IllegalInputException(e, UiElement.illegalTimeoutValue2, timeoutText);
        }
    }

    private class WaitingWrapperPane extends JPanel implements SupplierThrowing<Waiting, IllegalInputException> {

        private final WaitingEditPane waitingEditPane;

        public WaitingWrapperPane(final WaitingEditPane waitingEditPane) {
            this.waitingEditPane = waitingEditPane;
            SwingUtil.putComponentsToVerticalGrid(this, -1, createButtonsPane(), waitingEditPane);
        }

        public WaitingWrapperPane(final State state) {
            this(new WaitingEditPane(state));
        }

        private JPanel createButtonsPane() {
            final JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            panel.setSize(panel.getPreferredSize());
            panel.add(new JLabel(UiElement.waitingLabel.forUserLocale()));
            panel.add(createMoveUpButton());
            panel.add(createMoveDownButton());
            panel.add(createAddButton());
            panel.add(createDeleteButton());
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

        @Override
        public Waiting get() throws IllegalInputException {
            return waitingEditPane.get();
        }
    }
}
