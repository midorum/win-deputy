package midorum.win32.deputy.ui;

import dma.function.SupplierThrowing;
import midorum.win32.deputy.i18n.UiElement;
import midorum.win32.deputy.model.Activity;
import midorum.win32.deputy.model.IllegalInputException;

import javax.swing.*;

class ActivityViewPane extends JPanel implements SupplierThrowing<Activity, IllegalInputException> {

    private final Activity activity;

    ActivityViewPane(final Activity activity) {
        this.activity = activity;
        final JLabel titleLabel = new JLabel(activity.getTitle());
        final JLabel descriptionLabel = new JLabel(activity.getDescription().orElse(null));
        final JCheckBox repeatableCheckBox = new JCheckBox(UiElement.repeatableLabel.forUserLocale(),
                activity.isRepeatable());
        repeatableCheckBox.setEnabled(false);
        SwingUtil.putComponentsToVerticalGrid(this,
                -1,
                titleLabel,
                descriptionLabel,
                repeatableCheckBox);
    }

    @Override
    public Activity get() throws IllegalInputException {
        return activity;
    }
}
