#Script file used for creating an image onto a USB drive of the 240GB SSD (Main)

echo Creating an Image file of the main SSD......

sudo dd if=/dev/sda of=/dev/sdc bs=64K conv=noerror,sync

echo Creation of Backup Image done .....
