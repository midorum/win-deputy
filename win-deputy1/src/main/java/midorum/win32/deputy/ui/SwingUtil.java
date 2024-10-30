package midorum.win32.deputy.ui;

import java.awt.*;

final class SwingUtil {

    private SwingUtil() {
    }

    public static void retainComponentSize(final Component component) {
        final Dimension maximumSize = component.getMaximumSize();
        final Dimension preferredSize = component.getPreferredSize();
        preferredSize.setSize(maximumSize.getWidth(), preferredSize.getHeight());
        component.setMaximumSize(preferredSize);
    }

    public static void putComponentsToVerticalGrid(final Container container, final double[] weights, final Component... components) {
        if (container == null || weights == null || components == null || weights.length != components.length)
            throw new IllegalArgumentException();
        if (!(container.getLayout() instanceof GridBagLayout)) {
            container.setLayout(new GridBagLayout());
        }
        final GridBagConstraints c = new GridBagConstraints();
//        c.anchor = GridBagConstraints.FIRST_LINE_START; // align to the top left
        c.anchor = GridBagConstraints.PAGE_START; // align to the top
        c.weightx = 0.5; // fill all available width
        c.gridy = 0; // first grid row
        for (int i = 0; i < components.length; i++) {
            if (weights[i] > 0.0) {
//                c.fill = GridBagConstraints.HORIZONTAL; // resize component horizontally
                c.fill = GridBagConstraints.BOTH; // resize component in both directions
                c.weighty = weights[i]; // fill all available height
            } else {
                c.fill = GridBagConstraints.HORIZONTAL; // resize component horizontally
                c.weighty = 0.0; // do not fill all available height
            }
            container.add(components[i], c);
            c.gridy++; // increment grid row
        }
    }

    public static void putComponentsToVerticalGrid(final Container container, int stretch, final Component... components) {
        if (!(container.getLayout() instanceof GridBagLayout)) {
            container.setLayout(new GridBagLayout());
        }
        final GridBagConstraints c = new GridBagConstraints();
//        c.anchor = GridBagConstraints.FIRST_LINE_START; // align to the top left
        c.anchor = GridBagConstraints.PAGE_START; // align to the top
        c.weightx = 0.5; // fill all available width
        c.gridy = 0; // first grid row
        final int stretchIndex = stretch == Integer.MAX_VALUE ? components.length - 1 : stretch;
        for (int i = 0; i < components.length; i++) {
            if (i == stretchIndex) {
                c.fill = GridBagConstraints.BOTH; // resize component in both directions
                c.weighty = 0.5; // fill all available height
            } else if (stretchIndex == -1 && i == components.length - 1) {
                c.fill = GridBagConstraints.HORIZONTAL; // resize component horizontally
                c.weighty = 0.5; // fill all available height
            } else {
                c.fill = GridBagConstraints.HORIZONTAL; // resize component horizontally
                c.weighty = 0.0; // do not fill all available height
            }
            container.add(components[i], c);
            c.gridy++; // increment grid row
        }
    }

    public static void putComponentsToVerticalGrid(final Container container, final Component... components) {
        putComponentsToVerticalGrid(container, Integer.MAX_VALUE, components);
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

    public static void putComponentsToHorizontalGrid(final Container container, final int stretch, final Component... components) {
        if (!(container.getLayout() instanceof GridBagLayout)) {
            container.setLayout(new GridBagLayout());
        }
        final GridBagConstraints c = new GridBagConstraints();
        c.anchor = GridBagConstraints.LINE_START; // align to left
        c.gridx = 0; // first grid column
        c.weighty = 0.0; // do not fill all available height
        final int stretchIndex = stretch == Integer.MAX_VALUE ? components.length - 1 : stretch;
        for (int i = 0; i < components.length; i++) {
            if (i == stretchIndex) {
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

    public static void putComponentsToHorizontalGrid(final Container container, final double[] weights, final Component... components) {
        if (container == null || weights == null || components == null || weights.length != components.length)
            throw new IllegalArgumentException();
        if (!(container.getLayout() instanceof GridBagLayout))
            container.setLayout(new GridBagLayout());
        final GridBagConstraints c = new GridBagConstraints();
        c.anchor = GridBagConstraints.LINE_START; // align to left
        c.gridx = 0; // first grid column
        c.weighty = 0.0; // do not fill all available height
        for (int i = 0; i < components.length; i++) {
            if (weights[i] > 0.0) {
                c.fill = GridBagConstraints.HORIZONTAL; // resize component horizontally
                c.weightx = weights[i]; // fill all available width
            } else {
                c.fill = GridBagConstraints.NONE; // do not resize component
                c.weightx = 0.0; // do not fill all available width
            }
            container.add(components[i], c);
            c.gridx++; // increment grid column
        }
    }

}
