# About /Data/ and git

- `/Data/` folder is ignored by git.
- The expected directory is:
    - `./Data/MC1 Data/<datafile>` _ie._ `CGCS-GraphData.zip.001`, with index spanning .001 thru .016.
        - .001 thru .016 apeer to be incorrectly tagged, and appear not to be either .zip or .csv??
        - I have placed the 5.9 GB `./Data/MC1 Data/CGCS-GraphData.csv` inside as well.
    - `./Data/MC2-Image-Data/PersonNN` where NN is an unpadded index 1:40,.
        - inside `./Data/MC2-Image-Data/PersonNN` there are varying number (order 10) of `PersonNN_MM` .csv AND .jpgs, where mm is an unpadded 1:max(MM).
        - _ie._ `./Data/MC2-Image-Data/PersonNN/Person1_1.csv` thru `./Data/MC2-Image-Data/PersonNN/Person1_11.csv`, 11 .csv and .jpg files for person1.
    - MC3 doesn't have data, only a submission form, and so will live in `./Submissions/`.

--- Nick Spyrison 04/04/2020
