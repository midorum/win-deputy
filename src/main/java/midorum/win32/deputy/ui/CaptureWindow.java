package midorum.win32.deputy.ui;

import com.midorum.win32api.facade.Rectangle;
import com.midorum.win32api.facade.Win32System;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.image.BufferedImage;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

class CaptureWindow {

    public static final float OPACITY_DEFAULT = 0.55f;
    private final Consumer<Optional<Rectangle>> consumer;
    private final boolean forDefaultScreenDevice;
    private final boolean doNotMoveOutOfScreenEdges;

    public CaptureWindow(Consumer<Optional<Rectangle>> consumer, boolean forDefaultScreenDevice) {
        this.consumer = consumer;
        this.forDefaultScreenDevice = forDefaultScreenDevice;
        this.doNotMoveOutOfScreenEdges = true;
    }

    public CaptureWindow(Consumer<Optional<BufferedImage>> consumer) {
        this(maybeRectangle -> maybeRectangle.ifPresentOrElse(rectangle ->
                        consumer.accept(Optional.ofNullable(Win32System.getInstance().getScreenShotMaker().takeRectangle(rectangle))),
                () -> consumer.accept(Optional.empty())), true);
    }

    public void display() {
        final JFrame frame = constructFrame();
        SwingUtilities.invokeLater(() -> {
            frame.setOpacity(OPACITY_DEFAULT);
            frame.setVisible(true);
        });
    }

    private JFrame constructFrame() {
        final JFrame frame = new JFrame();
        frame.setSize(300, 200);
        frame.setLocationRelativeTo(null);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setUndecorated(true);
        frame.setFocusable(true);

        final Action captureRegion = () -> {
            final Point locationOnScreen = frame.getLocationOnScreen();
            final Dimension size = frame.getSize();
            frame.dispose();
            this.consumer.accept(Optional.of(getRectangle(locationOnScreen.x, locationOnScreen.y, locationOnScreen.x + size.width, locationOnScreen.y + size.height)));
        };

        final Action closeFrame = () -> {
            frame.dispose();
            this.consumer.accept(Optional.empty());
        };

        final ColorHolder colorHolder = new ColorHolder(color -> frame.getContentPane().setBackground(color), frame.getContentPane().getBackground());
        final OpacityHolder opacityHolder = new OpacityHolder(frame::setOpacity, OPACITY_DEFAULT);
        final JPopupMenu menu = createMenu(closeFrame, captureRegion, colorHolder, opacityHolder);
        final MouseEventListener mouseEventListener = new MouseEventListener(menu);
        frame.addMouseListener(mouseEventListener);
        frame.addMouseMotionListener(mouseEventListener);
        frame.addMouseWheelListener(mouseEventListener);
        frame.addKeyListener(new KeyEventListener(closeFrame, captureRegion, menu, colorHolder, opacityHolder, doNotMoveOutOfScreenEdges));
        return frame;
    }

    private Rectangle getRectangle(final int left, final int top, final int right, final int bottom) {
        if (!this.forDefaultScreenDevice) return new Rectangle(left, top, right, bottom);
        final Dimension awtResolution = Toolkit.getDefaultToolkit().getScreenSize();
        final DisplayMode currentDisplayMode = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice().getDisplayMode();
        final double xFactor = currentDisplayMode.getWidth() / awtResolution.getWidth();
        final double yFactor = currentDisplayMode.getHeight() / awtResolution.getHeight();
        return new Rectangle(
                (int) (left * xFactor),
                (int) (top * yFactor),
                (int) (right * xFactor),
                (int) (bottom * yFactor));
    }

    private JPopupMenu createMenu(final Action closeFrame, final Action captureRegion, final ColorHolder colorHolder, final OpacityHolder opacityHolder) {
        final JPopupMenu popupMenu = new JPopupMenu();
        popupMenu.add(createMenuItem("Capture Ctrl+Enter", e -> captureRegion.perform()));
        popupMenu.addSeparator();
        popupMenu.add(createSubMenu("Color Ctrl+C",
                createMenuItem("Default", e -> colorHolder.setDefault()),
                createMenuItem("White", e -> colorHolder.white()),
                createMenuItem("Light Gray", e -> colorHolder.lightGray()),
                createMenuItem("Gray", e -> colorHolder.gray()),
                createMenuItem("Dark Gray", e -> colorHolder.darkGray()),
                createMenuItem("Red", e -> colorHolder.red()),
                createMenuItem("Blue", e -> colorHolder.blue()),
                createMenuItem("Green", e -> colorHolder.green()),
                createMenuItem("Yellow", e -> colorHolder.yellow())
        ));
        popupMenu.add(createSubMenu("Opaque Ctrl+O/Ctrl+P",
                createMenuItem("Default", e -> opacityHolder.setDefault()),
                createMenuItem("90%", e -> opacityHolder.set(0.9f)),
                createMenuItem("80%", e -> opacityHolder.set(0.8f)),
                createMenuItem("70%", e -> opacityHolder.set(0.7f)),
                createMenuItem("60%", e -> opacityHolder.set(0.6f)),
                createMenuItem("50%", e -> opacityHolder.set(0.5f)),
                createMenuItem("40%", e -> opacityHolder.set(0.4f)),
                createMenuItem("30%", e -> opacityHolder.set(0.3f)),
                createMenuItem("20%", e -> opacityHolder.set(0.2f)),
                createMenuItem("10%", e -> opacityHolder.set(0.1f))
        ));
        popupMenu.addSeparator();
        popupMenu.add(createMenuItem("Close Ctrl+Z", e -> closeFrame.perform()));
        return popupMenu;
    }

    private JMenu createSubMenu(String text, JMenuItem... menuItems) {
        final JMenu sectionsMenu = new JMenu(text);
        for (JMenuItem menuItem : menuItems) {
            sectionsMenu.add(menuItem);
        }
        return sectionsMenu;
    }

    private JMenuItem createMenuItem(String text, ActionListener actionListener) {
        final JMenuItem menuItemClose = new JMenuItem(text);
        menuItemClose.addActionListener(actionListener);
        return menuItemClose;
    }

    private class MouseEventListener implements MouseListener, MouseMotionListener, MouseWheelListener {

        private Point startDragScreenMouseLocation;
        private Point startDragScreenFrameLocation;
        private final JPopupMenu popupMenu;

        private MouseEventListener(JPopupMenu popupMenu) {
            this.popupMenu = popupMenu;
        }

        private Point getScreenMouseLocation(MouseEvent e) {
            final Point relativeMouseLocation = e.getPoint();
            final Point screenFrameLocation = e.getComponent().getLocationOnScreen();
            return new Point((int) (screenFrameLocation.getX() + relativeMouseLocation.getX()),
                    (int) (screenFrameLocation.getY() + relativeMouseLocation.getY()));
        }

        @Override
        public void mouseClicked(MouseEvent e) {
        }

        @Override
        public void mousePressed(MouseEvent e) {
            if (e.getButton() == MouseEvent.BUTTON1) {
                this.startDragScreenMouseLocation = this.getScreenMouseLocation(e);
                this.startDragScreenFrameLocation = e.getComponent().getLocation();
            }
        }

        @Override
        public void mouseReleased(MouseEvent e) {
            if (e.getButton() == MouseEvent.BUTTON3) {
                popupMenu.show(e.getComponent(), e.getX(), e.getY());
            }
        }

        @Override
        public void mouseEntered(MouseEvent e) {
        }

        @Override
        public void mouseExited(MouseEvent e) {
        }

        @Override
        public void mouseDragged(MouseEvent e) {
            if ((e.getModifiersEx() & InputEvent.BUTTON1_DOWN_MASK) == InputEvent.BUTTON1_DOWN_MASK) {
                getMouseLocationOffset(e).ifPresent(mouseLocationOffset ->
                        e.getComponent().setLocation(new Point(
                                (int) (this.startDragScreenFrameLocation.getX() + mouseLocationOffset.getX()),
                                (int) (this.startDragScreenFrameLocation.getY() + mouseLocationOffset.getY()))));
            }
        }

        private Optional<Point> getMouseLocationOffset(MouseEvent e) {
            final KeyMask defaultMask = KeyMask.button1Mask;
            final KeyMask roughMask = KeyMask.button1CtrlMask;
            final KeyMask accurateMask = KeyMask.button1CtrlAltMask;
            final int modifiersEx = e.getModifiersEx();
            if (defaultMask.isSet(modifiersEx)) {
                return getNewScreenMouseLocation(e, 1);
            } else if (roughMask.isSet(modifiersEx)) {
                return getNewScreenMouseLocation(e, 2);
            } else if (accurateMask.isSet(modifiersEx)) {
                return getNewScreenMouseLocation(e, 10);
            }
            return Optional.empty();
        }

        private Optional<Point> getNewScreenMouseLocation(MouseEvent e, int factor) {
            final Point currentScreenMouseLocation = this.getScreenMouseLocation(e);
            return Optional.of(new Point(
                    calculatePositionX((int) currentScreenMouseLocation.getX(), factor),
                    calculatePositionY((int) currentScreenMouseLocation.getY(), factor)));
        }

        private int calculatePositionY(int y, int factor) {
            return (y - (int) startDragScreenMouseLocation.getY()) / factor;
        }

        private int calculatePositionX(int x, int factor) {
            return (x - (int) startDragScreenMouseLocation.getX()) / factor;
        }

        @Override
        public void mouseMoved(MouseEvent e) {
        }

        @Override
        public void mouseWheelMoved(MouseWheelEvent e) {
            final KeyMask roughVerticalMask = KeyMask.ctrlMask;
            final KeyMask accurateVerticalMask = KeyMask.ctrlAltMask;
            final KeyMask roughHorizontalMask = KeyMask.shiftMask;
            final KeyMask accurateHorizontalMask = KeyMask.shiftAltMask;
            final int modifiersEx = e.getModifiersEx();
            if (roughVerticalMask.isSet(modifiersEx)) {
                resizeVertically(e, 10 * e.getWheelRotation());
            } else if (accurateVerticalMask.isSet(modifiersEx)) {
                resizeVertically(e, e.getWheelRotation());
            } else if (roughHorizontalMask.isSet(modifiersEx)) {
                resizeHorizontally(e, 10 * e.getWheelRotation());
            } else if (accurateHorizontalMask.isSet(modifiersEx)) {
                resizeHorizontally(e, e.getWheelRotation());
            }
        }

        private void resizeVertically(MouseWheelEvent e, int delta) {
            final Component component = e.getComponent();
            final Dimension size = component.getSize();
            component.setSize(size.width, Math.max(size.height + delta, 1));
        }

        private void resizeHorizontally(MouseWheelEvent e, int delta) {
            final Component component = e.getComponent();
            final Dimension size = component.getSize();
            component.setSize(Math.max(size.width + delta, 1), size.height);
        }
    }

    private record KeyEventListener(
            Action closeFrame,
            Action makeScreenShot,
            JPopupMenu popupMenu,
            ColorHolder colorHolder,
            OpacityHolder opacityHolder,
            boolean doNotMoveOutOfScreenEdges
    ) implements KeyListener {

        @Override
        public void keyTyped(KeyEvent e) {
            //System.out.println(e);
        }

        @Override
        public void keyPressed(KeyEvent e) {
            //System.out.println(e);
            final int keyCode = e.getKeyCode();
            KeyMask.get(e.getModifiersEx()).ifPresent(keyMask -> {
                switch (keyMask) {
                    case ctrlMask -> {
                        switch (keyCode) {
                            case KeyEvent.VK_UP -> moveComponent(e.getComponent(), 0, -10);
                            case KeyEvent.VK_DOWN -> moveComponent(e.getComponent(), 0, 10);
                            case KeyEvent.VK_LEFT -> moveComponent(e.getComponent(), -10, 0);
                            case KeyEvent.VK_RIGHT -> moveComponent(e.getComponent(), 10, 0);
                        }
                    }
                    case ctrlAltMask -> {
                        switch (keyCode) {
                            case KeyEvent.VK_UP -> moveComponent(e.getComponent(), 0, -1);
                            case KeyEvent.VK_DOWN -> moveComponent(e.getComponent(), 0, 1);
                            case KeyEvent.VK_LEFT -> moveComponent(e.getComponent(), -1, 0);
                            case KeyEvent.VK_RIGHT -> moveComponent(e.getComponent(), 1, 0);
                        }
                    }
                    case shiftMask -> {
                        switch (keyCode) {
                            case KeyEvent.VK_UP -> resizeVertically(e.getComponent(), -10);
                            case KeyEvent.VK_DOWN -> resizeVertically(e.getComponent(), 10);
                            case KeyEvent.VK_LEFT -> resizeHorizontally(e.getComponent(), -10);
                            case KeyEvent.VK_RIGHT -> resizeHorizontally(e.getComponent(), 10);
                        }
                    }
                    case shiftAltMask -> {
                        switch (keyCode) {
                            case KeyEvent.VK_UP -> resizeVertically(e.getComponent(), -1);
                            case KeyEvent.VK_DOWN -> resizeVertically(e.getComponent(), 1);
                            case KeyEvent.VK_LEFT -> resizeHorizontally(e.getComponent(), -1);
                            case KeyEvent.VK_RIGHT -> resizeHorizontally(e.getComponent(), 1);
                        }
                    }
                }
            });
        }

        @Override
        public void keyReleased(KeyEvent e) {
            //System.out.println(e);
            final int keyCode = e.getKeyCode();
            KeyMask.get(e.getModifiersEx()).ifPresent(keyMask -> {
                if (keyMask == KeyMask.ctrlMask) {
                    switch (keyCode) {
                        case KeyEvent.VK_ENTER -> this.makeScreenShot.perform();
                        case KeyEvent.VK_Z -> this.closeFrame.perform();
                        case KeyEvent.VK_M -> this.popupMenu.show(e.getComponent(), 0, 0);
                        case KeyEvent.VK_C -> this.colorHolder.next();
                        case KeyEvent.VK_O -> this.opacityHolder.next();
                        case KeyEvent.VK_P -> this.opacityHolder.previous();
                    }
                }
            });
        }

        private void moveComponent(Component component, int x, int y) {
            final Point currentLocation = component.getLocation();
            final Point locationOnScreen = component.getLocationOnScreen();
            final Dimension size = component.getSize();
            final Container parent = component.getParent();
//            SwingUtilities.convertPointToScreen(point, component);
//            System.out.println("currentLocation: " + currentLocation + " locationOnScreen: " + locationOnScreen + " size:" + size);
//            System.out.println("parent: "+parent);
            final int xa = (int) currentLocation.getX();
            final int ya = (int) currentLocation.getY();
            if (doNotMoveOutOfScreenEdges) {
                component.setLocation(new Point(xa <= 0 && x < 0 ? 0 : xa + x, ya <= 0 && y < 0 ? 0 : ya + y));
            } else {
                component.setLocation(new Point(xa + x, ya + y));
            }
        }

        private void resizeVertically(Component component, int delta) {
            final Dimension size = component.getSize();
            component.setSize(size.width, Math.max(size.height + delta, 1));
        }

        private void resizeHorizontally(Component component, int delta) {
            final Dimension size = component.getSize();
            component.setSize(Math.max(size.width + delta, 1), size.height);
        }
    }

    private enum KeyMask {
        ctrlMask(InputEvent.CTRL_DOWN_MASK, InputEvent.SHIFT_DOWN_MASK | InputEvent.ALT_DOWN_MASK),
        ctrlAltMask(InputEvent.CTRL_DOWN_MASK | InputEvent.ALT_DOWN_MASK, InputEvent.SHIFT_DOWN_MASK),
        shiftMask(InputEvent.SHIFT_DOWN_MASK, InputEvent.CTRL_DOWN_MASK | InputEvent.ALT_DOWN_MASK),
        shiftAltMask(InputEvent.SHIFT_DOWN_MASK | InputEvent.ALT_DOWN_MASK, InputEvent.CTRL_DOWN_MASK),
        button1Mask(InputEvent.BUTTON1_DOWN_MASK, InputEvent.CTRL_DOWN_MASK | InputEvent.SHIFT_DOWN_MASK | InputEvent.ALT_DOWN_MASK),
        button1CtrlMask(InputEvent.BUTTON1_DOWN_MASK | InputEvent.CTRL_DOWN_MASK, InputEvent.SHIFT_DOWN_MASK | InputEvent.ALT_DOWN_MASK),
        button1CtrlAltMask(InputEvent.BUTTON1_DOWN_MASK | InputEvent.CTRL_DOWN_MASK | InputEvent.ALT_DOWN_MASK, InputEvent.SHIFT_DOWN_MASK);

        final int onMask;
        final int offMask;

        KeyMask(final int onMask, final int offMask) {
            this.onMask = onMask;
            this.offMask = offMask;
        }

        public boolean isSet(final int modifiers) {
            return (modifiers & (this.onMask | this.offMask)) == this.onMask;
        }

        public static Optional<KeyMask> get(final int modifiers) {
            return Optional.ofNullable(
                    ctrlMask.isSet(modifiers) ? ctrlMask
                            : ctrlAltMask.isSet(modifiers) ? ctrlAltMask
                            : shiftMask.isSet(modifiers) ? shiftMask
                            : shiftAltMask.isSet(modifiers) ? shiftAltMask
                            : button1Mask.isSet(modifiers) ? button1Mask
                            : button1CtrlMask.isSet(modifiers) ? button1CtrlMask
                            : button1CtrlAltMask.isSet(modifiers) ? button1CtrlAltMask
                            : null);
        }
    }

    private static class ColorHolder {
        final Consumer<Color> colorConsumer;
        final Color defaultColor;
        final Color[] colors;
        final AtomicInteger current;

        public ColorHolder(final Consumer<Color> colorConsumer, final Color defaultColor) {
            this.colorConsumer = colorConsumer;
            this.defaultColor = defaultColor;
            colors = new Color[]{defaultColor, Color.WHITE, Color.LIGHT_GRAY, Color.GRAY, Color.DARK_GRAY, Color.RED, Color.BLUE, Color.GREEN, Color.YELLOW};
            current = new AtomicInteger(0);
        }

        public void set(final Color color) {
            this.colorConsumer.accept(color);
        }

        public void setDefault() {
            set(defaultColor);
        }

        public void white() {
            set(Color.WHITE);
        }

        public void lightGray() {
            set(Color.LIGHT_GRAY);
        }

        public void gray() {
            set(Color.GRAY);
        }

        public void darkGray() {
            set(Color.DARK_GRAY);
        }

        public void red() {
            set(Color.RED);
        }

        public void blue() {
            set(Color.BLUE);
        }

        public void green() {
            set(Color.GREEN);
        }

        public void yellow() {
            set(Color.YELLOW);
        }

        public void next() {
            int i = current.incrementAndGet();
            if (i >= colors.length) {
                current.set(i = 0);
            }
            set(colors[i]);
        }
    }

    private static class OpacityHolder {
        final Consumer<Float> opacityConsumer;
        final float defaultOpacity;
        final float[] opacities;
        final AtomicInteger current;

        private OpacityHolder(Consumer<Float> opacityConsumer, float defaultOpacity) {
            this.opacityConsumer = opacityConsumer;
            this.defaultOpacity = defaultOpacity;
            this.opacities = new float[]{defaultOpacity, 0.9f, 0.8f, 0.7f, 0.6f, 0.5f, 0.4f, 0.3f, 0.2f, 0.1f};
            this.current = new AtomicInteger(0);
        }

        public void setDefault() {
            set(defaultOpacity);
        }

        public void set(final float opacity) {
            this.opacityConsumer.accept(opacity);
        }

        public void next() {
            int i = current.incrementAndGet();
            if (i >= opacities.length) {
                current.set(i = 0);
            }
            set(opacities[i]);
        }

        public void previous() {
            int i = current.decrementAndGet();
            if (i <= 0) {
                current.set(i = opacities.length - 1);
            }
            set(opacities[i]);
        }

    }

    private interface Action {

        void perform();
    }
}
