package midorum.win32.deputy.ui;

import com.midorum.win32api.facade.Rectangle;
import com.midorum.win32api.facade.Win32System;
import midorum.win32.deputy.model.SourceType;

import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

class SourceTypeEditPane extends JPanel {

    public SourceTypeEditPane(final List<SourceType> sourceTypes, final String dataValue) {
        final List<Component> components = new ArrayList<>();
        if (sourceTypes.contains(SourceType.userInput)) {
            final JTextField valueField = new JTextField(dataValue);
            components.add(valueField);
        } else {
            final JLabel label = new JLabel(dataValue);
            components.add(label);
        }
        if (sourceTypes.contains(SourceType.pickFile)) {
            final Button btn = new Button("...");
            btn.addActionListener(e -> System.out.println(e));
            components.add(btn);
        }
        if (sourceTypes.contains(SourceType.makeShot)) {
            final Button btn = new Button("[o]");
            btn.addActionListener(e -> new CaptureWindow(maybeRectangle -> maybeRectangle.ifPresentOrElse(rectangle -> {
                // todo ask file name
                saveImage(Win32System.getInstance().getScreenShotMaker()
                        .takeRectangle(convertRectangleToCurrentResolution(rectangle)), askFileName().orElse(getDefaultFileName()));
            }, () -> JOptionPane.showMessageDialog(this, "No rectangle was captured"))).display());
            components.add(btn);
        }
        Util.putComponentsToHorizontalGridStretchFirst(this, components.toArray(Component[]::new));
    }

    private String getDefaultFileName() {
        return "shot_" + System.currentTimeMillis();
    }

    private Optional<String> askFileName() {
        final JFileChooser fileChooser = new JFileChooser();
        int returnValue = fileChooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            File selectedFile = fileChooser.getSelectedFile();
//            System.out.println("selected file: " + selectedFile.getAbsolutePath());
//            System.out.println("selected file exists: " + selectedFile.exists());
//            System.out.println("selected file name: " + selectedFile.getName());
//            System.out.println("selected file path: " + selectedFile.getParentFile().getAbsolutePath());
            if (!selectedFile.exists() || JOptionPane.showConfirmDialog(this, "Do you want to replace existing file?") == 1) {
                return Optional.of(selectedFile.getAbsolutePath());
            }
        }
        return Optional.empty();
    }

    private Rectangle convertRectangleToCurrentResolution(final Rectangle awtRectangle) {
        final Dimension awtResolution = Toolkit.getDefaultToolkit().getScreenSize();
        final DisplayMode currentDisplayMode = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice().getDisplayMode();
        final double xFactor = currentDisplayMode.getWidth() / awtResolution.getWidth();
        final double yFactor = currentDisplayMode.getHeight() / awtResolution.getHeight();
        return new Rectangle(
                (int) (awtRectangle.left() * xFactor),
                (int) (awtRectangle.top() * yFactor),
                (int) (awtRectangle.right() * xFactor),
                (int) (awtRectangle.bottom() * yFactor));
    }

    private void saveImage(final BufferedImage bufferedImage, final String name) {
        try {
            new FileServiceProvider()
                    .withFile("wrong_shots" + File.separator + name.replaceAll("[<>:\"/\\\\|?*\s]", "_"), "png")
                    .writeImage(bufferedImage);
        } catch (IOException e) {
            System.out.println("error while saving image:" + e.getMessage());
        }
    }
}
