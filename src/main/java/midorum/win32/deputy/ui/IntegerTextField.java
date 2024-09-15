package midorum.win32.deputy.ui;

import javax.swing.*;
import javax.swing.text.DefaultFormatterFactory;
import javax.swing.text.NumberFormatter;
import java.text.NumberFormat;
import java.text.ParseException;

public class IntegerTextField extends JFormattedTextField {

    public IntegerTextField(final int lowBound, final int topBound) {
        final NumberFormat format = NumberFormat.getInstance();
        format.setGroupingUsed(false);
        final NumberFormatter formatter = new NumberFormatter(format) {
            @Override
            public Object stringToValue(final String text) throws ParseException {
                if (text.isEmpty()) return null;
                return super.stringToValue(text);
            }
        };
        formatter.setValueClass(Integer.class);
        formatter.setMaximum(topBound);
        formatter.setMinimum(lowBound);
        formatter.setAllowsInvalid(false);
        formatter.setCommitsOnValidEdit(true);
        setFormatterFactory(new DefaultFormatterFactory(formatter));
        setHorizontalAlignment(SwingConstants.CENTER);
    }

}
