package midorum.win32.deputy.ui;

import midorum.win32.deputy.model.TaskDispatcher;

import javax.swing.*;
import javax.swing.event.HyperlinkEvent;
import java.awt.*;

class CreditsPane extends JPanel {

    private static final int PANE_MARGIN = 10;
    private final String header = "Credits";
    private final String iconCredits = "Icon: <a href=\"https://iconscout.com/icons/droid-robot\">Droid Robot</a>" +
            " by <a href=\"https://iconscout.com/contributors/zach\">Flaticons</a>" +
            " on <a href=\"https://iconscout.com\">IconScout</a>";
    private final TaskDispatcher taskDispatcher;

    CreditsPane(final TaskDispatcher taskDispatcher, final UiUtil uiUtil) {
        this.taskDispatcher = taskDispatcher;
        final JEditorPane pane = new JEditorPane();
        pane.setContentType("text/html");//set content as html
        pane.setEditable(false);
        pane.setOpaque(false);
        pane.addHyperlinkListener(hle -> {
            if (HyperlinkEvent.EventType.ACTIVATED.equals(hle.getEventType())) {
                try {
                    Desktop.getDesktop().browse(hle.getURL().toURI());
                } catch (Exception ex) {
                    uiUtil.logThrowable("error occurred while opening URL", ex);
                }
            }
        });
        pane.setText("<h3>"+ header + "</h3>"
                + "<p>"+ iconCredits + "</p>");
        SwingUtil.putComponentsToVerticalGrid(this, 0, pane, createButtonPane());
    }

    private JPanel createButtonPane() {
        final JPanel buttonPane = new JPanel();
        buttonPane.setLayout(new BoxLayout(buttonPane, BoxLayout.LINE_AXIS));
        buttonPane.setBorder(BorderFactory.createEmptyBorder(0, PANE_MARGIN, PANE_MARGIN, PANE_MARGIN));
        buttonPane.add(Box.createHorizontalGlue());
        buttonPane.add(createCloseButton());
        return buttonPane;
    }

    private Button createCloseButton() {
        final Button btn = new Button("Close");
        btn.addActionListener(e -> taskDispatcher.closeCredits());
        return btn;
    }

}
