# README #

This repository is in bitbucket being used for free and as such it has a maximum of 5 users. I only see it being used at most by 3 users.

### What is this repository for? ###

* Quick summary

  It holds the current sources for the port from COBOL application CTOS to running as GNUCobol (aka OpenCobol) application hosted on LINUX.

* Version
 
  Initial 17.2 and later

* [Learn Markdown](https://bitbucket.org/tutorials/markdowndemo)

### How do I get set up? ###

* Installing the Server
	
  1. Install Linux Mint Cinnamon (17.2 - our current version Sep 2016) making the main user ctools.
     From Update Manager install all the outstanding updates. This is shown by the 
     installing updates having an exclamation ! in the shield next to clock in the bottom
     right of the screen.
	
  2. From a terminal install the following using apt-get install
   - sudo apt-get install git git-gui g++ cmake 
	
   - For PUtty & pdftk see new instructions - 8 & 9. below - for installing on Linux mint 19.1 & greater
	 - as the apt-get does not bring back the latest software for Mint 19.1 as far as these two packages are concerned
	
  3. Using Software Manager install
    
   - putty - Only for Linux Mint 17.1-3
   - openssh server
   - sublime text
   - gtk 
   - GNUcobol 
   - pdftk
   - Geany
	 - Hylafax-server and hylafax-client
	
  4. Set up to retrieve the repository - note you must supply <user> and <password> in git clone below.
	
   - sudo mkdir /ctools
	
    4.1  Set all folders in ctools to ctools ownership.
      - cd /ctools
      - sudo chown -R ctools:ctools *
      - Also can do like this:    sudo chown ctools:ctools /ctools
  
      - mkdir /ctools/dev
      - cd /ctools/dev
      - git config --global user.email "Steve@Christensen.co.za"
      - git config --global user.name "Steve Christensen"
                         <username>   <pswd>
      - sudo git clone https://VincentRisi:Atl45514n@bitbucket.org/VincentRisi/ctos4ctools source
      - mkdir build
      - cd build
      - sudo cmake ../source
      - sudo make -k
      - sudo make install
	
  5. Change the key bindings using dconf-editor - you must first install:
	
   - sudo apt-get install dconf-tools
   - then run the dconf-editor in a terminal session and go to:
   - org
   - Cinamon
   - desktop
   - keybindings
   - wm
   - change all the entries in [ ] brackets so that the system does not interfere with the CTOS key strokes
	
  6. Setup putty.
	
   - in /ctools/dev/source/putty there are a number of files and folders that can be copied into /home/.putty to 
   - get the current config 
	 
  7. .bashrc needs to be copied to the users home directory with ALL the CTOS dependent entries in it.
   -  A copy is found in /ctools/dev/source/installs   Three types of file for auto-exit of putty or not ...
    
  8.  Installing pdftk Linux Mint 19.1 > - installation instructions also found here:
   -    https://linuxhint.com/install_pdftk_ubuntu/
	 - sudo add-apt-repository ppa:malteworld/ppa
	 - sudo apt update
	 - sudo apt install pdftk

   - OR for Mint 19.1 > sometimes the ppa:malteworld (also try ppa:malteworld1) has had pdftk removed as of 10/07/2020 so use:
   
   - Download pdftk-java_3.0.9-1_all.deb from https://packages.ubuntu.com/focal/pdftk-java  
         choose to download from za.archive.ubuntu.com/ubuntu in the first page of the link in red under Africa
   - sudo dpkg -i pdftk-java_3.0.9-1_all.deb
   - sudo apt-get install -f
	
  9. Installing PUtty on Linux Mint 19.1 >
	 - Download  putty-0.72.tar.gz from the PUtty website
   - Put the `putty-0.72.tar.gz` into the directory  `/home/ctools/Downloads`
	 - cd /home/ctools/Downloads
	 - tar vxf putty-0.72.tar.gz
	
	 - use menu on the pc by pressing the windows button and select Administrator/Synaptic Package Manager
	 - Select Search - it will pop up a Find dialog
	 - Look for Gtk selecting Look in Name Search
	 - Scroll down to libgtk-3-dev, mark it and the libgtk-3-doc for installation then hit the apply button.
	
	 - In a Terminal
	 - cd /home/ctools/Downloads/putty-0.72
	 - ./configure
	 - sudo make
	 - sudo make install
	
   - Use "Software Manager" to install putty

	 - putty should now work properly on the Linux box

  10. Setup all the new users 
    - Press windows key 
    - go to System Settings
    - go to Users & Groups
    - Add user with a temp password.  Make sure to add in ctools, lpadmin, plugdev,sambashare
        to the user groups...
    -  See the list of users in "UserList.txt" for ALL users on the Server currently.
	
  11. Set user passwords by 
    - su <enter>  (this to enter the root user which has powers to change user passwords)
	  - ctos (password)
	  -
    - passwd <username>
    - enter the temp password then enter a new password
    - See the List of passwords for users in "Logon.txt"
    
    - OR
    
    - Run the "/ctools/dev/source/CreatePasswordsAllUsers.sh" to set the whole lot in one go.

  12. Add all the users into the same group namely ctools
    - sudo usermod -g ctools <username>
    
    - OR

    - Run the "/ctools/dev/source/CreateGroupAllUsers.sh" to set ALL users to ctools group in one go.

  13. Change the WORKGROUP in Samba to allow printers to work in the group  
    - edit /etc/samba/smb.conf using sublime-text editor 
	  - look for the WORKGROUP entry and change it to CTJ
	  - change the Printer entries to allow browsing and guest ok = yes
	  - change the entry by removing the ; in front of the entry usershare max shares = 100
	
  14. Copy All the data off your memory stick USB from the ctools folder.
    - Make sure you do NOT copy the /ctools/bin, /ctools/dev, folders which have already been made in this new 
    - installation.
  
  15. Change permissions on the /ctools folder once 14. above has been done.  This will allow ALL users to access 
    - these sub folders.
    - cd /ctools
    - chmod 775 *
    - chmod -R g+rw *
	
  16. On the Server, to get the Right-Alt key (right Alt being Alt Gr) to work as a Left-Alt key in CTOS do the following:
    -  press <Windows key>
    -  Go to Settings
    -  Go to keyboard, then layouts tab, select the KBD on the left of the screen "English (South Africa)"
    -  then the Options tab, then "Alt/win behaviour" and change the default to "Alt and Meta are on Alt keys"
    -  then the right-Alt works like the Left-Alt key.....

  17.  Install TeamViewer.  32bit OS
    -  cd /tmp
    -  wget https://download.teamviewer.com/download/linux/signature/TeamViewer2017.asc
    -  sudo apt-key add TeamViewer2017.asc
    -  sudo sh -c 'echo "deb http://linux.teamviewer.com/deb stable main" >> /etc/apt/sources.list.d/teamviewer.list'
    -  sudo apt update
    -  sudo apt install teamviewer

       OR 64 BIT OS

    -  sudo apt update
    -  sudo apt install wget
    -  wget https://download.teamviewer.com/download/linux/TeamViewer_amd64.deb
    -  sudo apt install -y ./teamviewer_amd64.deb


  18.  Install Dropbox.
    -  Open "Software Manager" and search for Dropbox and install.

  19.  Setup Hylafax once connected to the fax modem.
    -  run "faxsetup"
    -  and you may need to run "faxaddmodem" when yo have a modem connected

  20.  Install ALL user .bashrc files having first made a copy by running "CreateBashrcEntries.sh" found on the server
    -  in /ctool/bin and also in /ctools/dev/source/CreateAllUsers.sh.
    -  This creates .bashrc files for each user and is stored in /ctools/users/  Edit .bashrc for ctools, steve, 
    -  john & vince to add the default entries now found in Mint 19.1 >.....  There is a working copy in /ctools/users 
    -  already so this step can be ignored if updating to Mint 19.1 - July 2020.

    -  On the new server run "CreateUpdateBashrcEntries.sh" which will copy the saved .bashrc files to their respective
    -  users home folders.

  21.  Install Deadlock utility
    -  sudo apt install db-util

    - Now you can run Deadlock.sh each morning the server is switched on.
    
  22. Setup Proxy Server - if running the serer @ CTJ through Kerio
    - Click on the network connection icon in the bottom right hand corner of the screen.
    - Click on "Proxy Settings"
    - Enter the following:
    -    "Method"      = Manual
    -    "http proxy"  = 192.168.100.254        3128
    -    "https proxy" = 192.168.100.254        3128
    -
    -  See /ctools/dev/source/ProxySettings.png
    -
  23. To setup the BackupHDD drive so that it mounts correctly 
   -   Copy the line in fstab-17.3Mint to the servers /etc/fstab file
   -     #
   -      UUID=08cb88af-11c3-4ed7-b320-ab018353c95a /BACKUPHDD      ext4    defaults        0       2
   -
  24. To add python pip & requests module to python 2.7 as the Email python script MailGunEmail.py uses the python requests module
   -  su ctools <return>
   -  ctos
   -  sudo apt install python-pip
   -
   -  su <return>
   -  ctos (as the password)
   -  sudo pip install -U requests
   -
   -  To see that requests has been installed type:
   -  pip show requests <return>
   -  this will get a number of lines prnted on the screen showing info about therequests module
   -
  25. To speed up the process of copying files to and from USB & to the BACKUPHDD do the following:
   -  edit using Sublimetext, the file /etc/sysctrl.conf and add the following lines to the top of the file.
   -  See the sample file in /ctools/dev/source/sysctrl.conf
   -
   -
   - vm.dirty_background_ratio = 5
   - vm.dirty_ratio = 10

   #
   # /etc/sysctl.conf - Configuration file for setting system variables
   # See /etc/sysctl.d/ for additional system variables.
   # See sysctl.conf (5) for information.
   #

   #kernel.domainname = example.com

   - fs.file-max = 2097152
   -
   -
   - ***END***
