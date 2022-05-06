import glob,os
for file in glob.glob('/ctools/gl/ctjgl*'):
    file2 = file.replace('ctjgl', 'ctj$2')
    os.rename(file, file2)
