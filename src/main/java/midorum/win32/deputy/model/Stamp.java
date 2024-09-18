package midorum.win32.deputy.model;

import com.midorum.win32api.facade.Rectangle;
import midorum.win32.deputy.common.CommonUtil;

import java.awt.image.BufferedImage;

public class Stamp {

    private final String name;
    private final int[] wholeData;
    private final Rectangle location;

    public Stamp(final String name, final int[] wholeData, final Rectangle location) {
        this.name = name;
        this.wholeData = wholeData;
        this.location = location;
    }

    public Stamp(final String name, final BufferedImage image) {
        this.name = name;
        this.wholeData = CommonUtil.imageToArray(image);
        this.location = new Rectangle(0, 0,
                image.getMinX() + image.getWidth(),
                image.getMinY() + image.getHeight());
    }

    public String name() {
        return name;
    }

    public int[] wholeData() {
        return wholeData;
    }

    public Rectangle location() {
        return location;
    }

}
