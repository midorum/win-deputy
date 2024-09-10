package midorum.win32.deputy.ui;

import midorum.win32.deputy.model.Activity;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

class ActivityEditPane extends JPanel {

    private JTextField titleField;
    private JTextArea descriptionField;
    private List<CheckWrapperPane> checks;
    private List<CommandWrapperPane> commands;
    private JPanel checksPane;
    private JPanel commandsPane;

    ActivityEditPane(final Activity activity) {
        checks = new ArrayList<>();
        checks.addAll(activity.getChecks().stream().map(CheckEditPane::new).map(CheckWrapperPane::new).collect(Collectors.toCollection(ArrayList::new)));
        commands = new ArrayList<>();
        commands.addAll(activity.getCommands().stream().map(CommandEditPane::new).map(CommandWrapperPane::new).collect(Collectors.toCollection(ArrayList::new)));

        compose(activity.getTitle(), activity.getDescription());
    }

    ActivityEditPane() {
        final String title = null;
        final String description = null;
        checks = new ArrayList<>();
        checks.add(new CheckWrapperPane());
        commands = new ArrayList<>();
        commands.add(new CommandWrapperPane());

        compose(title, description);
    }

    private void compose(final String title, final String description) {
//        setBorder(BorderFactory.createLineBorder(Color.ORANGE));
        titleField = new JTextField(title);
        descriptionField = new JTextArea(description);
        checksPane = new JPanel(new GridBagLayout());
//        checksPane.setBorder(BorderFactory.createLineBorder(Color.BLUE));
        commandsPane = new JPanel(new GridBagLayout());
//        commandsPane.setBorder(BorderFactory.createLineBorder(Color.GREEN));
        Util.putComponentsToVerticalGrid(this, titleField, descriptionField, checksPane, commandsPane);
        fillChecksGrid();
        fillCommandsGrid();
    }

    private void addCheckAbove(final CheckWrapperPane checkPane) {
        final int currentIndex = checks.indexOf(checkPane);
        checks.add(currentIndex, new CheckWrapperPane());
        repaintChecks();
    }

    private void deleteCheck(final CheckWrapperPane checkPane) {
        if (checks.size() == 1) {
            JOptionPane.showMessageDialog(this, "You cannot delete last check from activity");
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

    private void addCommandAbove(final CommandWrapperPane commandPane) {
        final int currentIndex = commands.indexOf(commandPane);
        commands.add(currentIndex, new CommandWrapperPane());
        repaintCommands();
    }

    private void deleteCommand(final CommandWrapperPane commandPane) {
        if (commands.size() == 1) {
            JOptionPane.showMessageDialog(this, "You cannot delete last check from activity");
            return;
        }
        commands.remove(commandPane);
        repaintCommands();
    }

    private void moveCommandUp(final CommandWrapperPane commandPane) {
        final int currentIndex = commands.indexOf(commandPane);
        if (currentIndex == 0) return;
        commands.remove(commandPane);
        commands.add(currentIndex - 1, commandPane);
        repaintCommands();
    }

    private void moveCommandDown(final CommandWrapperPane commandPane) {
        final int currentIndex = commands.indexOf(commandPane);
        if (currentIndex == commands.size() - 1) return;
        commands.remove(commandPane);
        commands.add(currentIndex + 1, commandPane);
        repaintCommands();
    }

    private void repaintCommands() {
        commandsPane.removeAll();
        fillCommandsGrid();
        revalidate();
    }

    private void fillChecksGrid() {
        Util.putComponentsToVerticalGrid(checksPane, checks.toArray(Component[]::new));
    }

    private void fillCommandsGrid() {
        Util.putComponentsToVerticalGrid(commandsPane, commands.toArray(Component[]::new));
    }

    private class CheckWrapperPane extends JPanel {

        private final CheckEditPane checkEditPane;

        private CheckWrapperPane(final CheckEditPane checkEditPane) {
            this.checkEditPane = checkEditPane;
//            setBorder(BorderFactory.createLineBorder(Color.BLUE));
            Util.putComponentsToVerticalGrid(this, createButtonsPane(), checkEditPane);
        }

        public CheckWrapperPane() {
            this(new CheckEditPane());
        }

        private JPanel createButtonsPane() {
            final JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            panel.setSize(panel.getPreferredSize());
//            panel.setBorder(BorderFactory.createLineBorder(Color.RED));
            panel.add(new JLabel("Check"));
            panel.add(createMoveUpButton());
            panel.add(createMoveDownButton());
            panel.add(createAddAboveButton());
            panel.add(createDeleteButton());
            return panel;
        }

        private Button createAddAboveButton() {
            final Button btn = new Button("+^");
            btn.addActionListener(e -> addCheckAbove(this));
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

    }

    private class CommandWrapperPane extends JPanel {

        private final CommandEditPane commandEditPane;

        private CommandWrapperPane(final CommandEditPane commandEditPane) {
            this.commandEditPane = commandEditPane;
//            setBorder(BorderFactory.createLineBorder(Color.BLUE));
            Util.putComponentsToVerticalGrid(this, createButtonsPane(), commandEditPane);
        }

        public CommandWrapperPane() {
            this(new CommandEditPane());
        }

        private JPanel createButtonsPane() {
            final JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            panel.setSize(panel.getPreferredSize());
//            panel.setBorder(BorderFactory.createLineBorder(Color.RED));
            panel.add(new JLabel("Command"));
            panel.add(createMoveUpButton());
            panel.add(createMoveDownButton());
            panel.add(createAddAboveButton());
            panel.add(createDeleteButton());
            return panel;
        }

        private Button createAddAboveButton() {
            final Button btn = new Button("+^");
            btn.addActionListener(e -> addCommandAbove(this));
            return btn;
        }

        private Button createDeleteButton() {
            final Button btn = new Button("x");
            btn.addActionListener(e -> deleteCommand(this));
            return btn;
        }

        private Button createMoveUpButton() {
            final Button btn = new Button("^");
            btn.addActionListener(e -> moveCommandUp(this));
            return btn;
        }

        private Button createMoveDownButton() {
            final Button btn = new Button("v");
            btn.addActionListener(e -> moveCommandDown(this));
            return btn;
        }

    }
}
