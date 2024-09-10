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
            btn.addActionListener(e -> new CaptureWindow(maybeImage -> maybeImage.ifPresentOrElse(image ->
                            Util.askNewFile(null, this).ifPresentOrElse(file -> {
                                final String fileName = file.getName();
                                final String filePath = file.getParentFile().getAbsolutePath();
                                saveImage(image, filePath, fileName);
                            }, () -> saveImage(image, null, Util.getDefaultFileName())),
                    () -> JOptionPane.showMessageDialog(this, "No rectangle was captured"))).display());
            components.add(btn);
        }
        Util.putComponentsToHorizontalGridStretchFirst(this, components.toArray(Component[]::new));
    }

    private void saveImage(final BufferedImage bufferedImage, final String path, final String name) {
        try {
            new FileServiceProvider()
                    .withFile(path, name, "png")
                    .writeImage(bufferedImage);
        } catch (IOException e) {
            JOptionPane.showMessageDialog(this, "error while saving image:" + e.getMessage());
        }
    }
}
