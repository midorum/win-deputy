package midorum.win32.deputy.ui;

import java.awt.*;
import java.util.Objects;

final class Util {
    private Util() {
    }

    public static void retainComponentSize(final Component component) {
        final Dimension maximumSize = component.getMaximumSize();
        final Dimension preferredSize = component.getPreferredSize();
        preferredSize.setSize(maximumSize.getWidth(), preferredSize.getHeight());
        component.setMaximumSize(preferredSize);
    }

    public static <T> int getIndex(T[] array, T t) {
        for (int i = 0; i < array.length; i++)
            if (Objects.equals(array[i], t)) return i;
        return -1;
    }


    public static void putComponentsToVerticalGrid(final Container container, final Component... components) {
        if (!(container.getLayout() instanceof GridBagLayout)) {
            container.setLayout(new GridBagLayout());
        }
        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL; // resize component horizontally
        c.anchor = GridBagConstraints.FIRST_LINE_START; // align to the top left
        c.gridy = 0; // first grid row
        c.weightx = 0.5; // fill all available width
        c.weighty = 0.0; // do not fill all available height
        for (int i = 0; i < components.length; i++) {
            if (i == components.length - 1) {
                c.fill = GridBagConstraints.BOTH; // resize component in both directions <<<<<<<
                c.weighty = 0.5; // fill all available height
            }
            container.add(components[i], c);
            c.gridy++; // increment grid row
        }
    }

    public static void putComponentsToHorizontalGrid(final Container container, final Component... components) {
        if (!(container.getLayout() instanceof GridBagLayout)) {
            container.setLayout(new GridBagLayout());
        }
        final GridBagConstraints c = new GridBagConstraints();
        c.anchor = GridBagConstraints.LINE_START; // align to left
        c.gridx = 0; // first grid column
        c.weightx = 0.0; // do not fill all available width
        c.weighty = 0.0; // do not fill all available height
        for (int i = 0; i < components.length; i++) {
            if (i == components.length - 1) {
                c.fill = GridBagConstraints.HORIZONTAL; // resize component horizontally
                c.weightx = 0.5; // fill all available width
            }
            container.add(components[i], c);
            c.gridx++; // increment grid column
        }
    }

    public static void putComponentsToHorizontalGridStretchFirst(final Container container, final Component... components) {
        if (!(container.getLayout() instanceof GridBagLayout)) {
            container.setLayout(new GridBagLayout());
        }
        final GridBagConstraints c = new GridBagConstraints();
        c.anchor = GridBagConstraints.LINE_START; // align to left
        c.gridx = 0; // first grid column
        c.weighty = 0.0; // do not fill all available height
        for (int i = 0; i < components.length; i++) {
            if (i ==  0) {
                c.fill = GridBagConstraints.HORIZONTAL; // resize component horizontally
                c.weightx = 0.5; // fill all available width
            } else {
                c.fill = GridBagConstraints.NONE; // do not resize component
                c.weightx = 0.0; // do not fill all available width
            }
            container.add(components[i], c);
            c.gridx++; // increment grid column
        }
    }

}
