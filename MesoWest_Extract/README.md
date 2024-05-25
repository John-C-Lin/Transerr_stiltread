R code to read in and process MesoWest met data


Below are some notes from emails discussing these MesoWest stations:

------------------------------------------------------------------------------------------------------------------------
On Fri, Mar 15, 2019 at 10:01 AM Alex Jacques <alexander.jacques@utah.edu> wrote:
Just one word of caution there that the list was picked based on the general ability of being reasonably representative of conditions (i.e. no giant obstacles in the way, exposure in all directions, etc.).  That said, they still do have their nuances...

1) The University of Utah ones mostly are shorter than 10m for winds, minus Flight Park South (FPS).  A few are on rooftops (WBB, NHMU, TRJO, NAA).
2) The Utah DOT ones should be pretty good for wind (~10m tall), but they are near highways, so temperatures might be a bit warmer than expected.

Just some things I figured I'd pass along, especially if related to evaluating model output.  Happy to answer any other questions as well.

Alexander Jacques
Postdoctoral Research Associate - Atmospheric Sciences/MesoWest
University of Utah
alexander.jacques@utah.edu
http://home.chpc.utah.edu/~u0790486/


On Fri, Mar 15, 2019 at 11:50 AM John C. Lin <John.Lin@utah.edu> wrote:
Perfect--thanks!


John


On 3/15/19 9:15 AM, Erik Crosman wrote:
John--

This email from Alex should contain the info you need for the BAMS sites for your HRRR validation.

Let us know if you have any questions.

Thanks,

E



Logan and John,

Erik forwarded along this request to John and I.  I've put together a listing of "key" MesoWest sites in the Excel spreadsheet that is attached.  I made my query a bit wider the figure window was showing, so there may be more sites than what will actually show in your figure.  Because of that, I wouldn't get deep into exact numbers here (not to mention definition of "key" site - if it comes up in reviews we could deal with at that juncture).

Couple things:

a) Both NAA and MTMET have the additional AQ sensors (pm and seasonal ozone) installed

b) MTMET also has the CL31 ceilometer 

c) MTMET also right now temporarily has the DAQ radiometer installed there, though I think that it may be going back to DAQ once their roof work is complete (Erik/John H. can you confirm?)

d) The continuous sodar we operate is located at lat/lon 40.74895, -112.03392

e) The DAQ sites are listed in the spreadsheet as well but I don't believe all are regulatory (Magna site just has weather sensors I think?) so I would perhaps check those with DAQ resources/contacts (their webpage perhaps).

As far as text to describe the weather stations in the figure... I would suggest the following (or similar to the following to fit the text)...

"Provided in this figure are key weather station locations operated by the National Weather Service, state agencies (e.g., Utah Department of Transportation and Division of Air Quality), and universities (e.g., University of Utah and Utah State University).  Additional weather stations also exist within the Salt Lake Valley and are operated by private citizens and other local organizations.  Weather observations from these platforms are retrieved and archived as part of the nationwide MesoWest data ingest and storage system (Horel et al. 2002) and distributed in real-time to government, research, the general public, and other entities via a suite of web-accessible pages and data API services.  Surface-based remote sensors within the valley are maintained by researchers within the University of Utah Atmospheric Sciences department (ceilometer and sodar) and the Utah Division of Air Quality (radiometer) that supplement the twice-daily rawinsondes launched by the National Weather Service at the Salt Lake International Airport.  A variety of web-based products are available to continuously monitor the boundary layer profiles of wind, temperature, and moisture from these remote sensors."

Horel et al. 2002 ref - http://journals.ametsoc.org/doi/abs/10.1175/1520-0477%282002%29083%3C0211%3AMCMITW%3E2.3.CO%3B2

I think that should cover it for the portion describing weather data assets in this paper.  Let us know if there are any other questions as well.

-Alex



July 9th, 2018 by John C. Lin (John.Lin@utah.edu)
Download MesoWest meteorological data from station near HDP
These stations would be:
1) Alta - Mt. Baldy (AMB)

To: "John C. Lin" <John.Lin@utah.edu>
Cc: Alex Jacques <alexander.jacques@utah.edu>, "Dr. John Horel" <john.horel@utah.edu>


Hi John,

Sure, all of the fixed site PM2.5, met, and ozone data shown on the websites I've made is accessible through products powered by MesoWest.  So that opens a couple of avenues for getting the data.

1)  You could get it from our MesoWest data download page at http://mesowest.utah.edu/cgi-bin/droman/download_ndb.cgi?stn=QHW.  That should give you access to both the met and air quality (PM2.5 and Ozone) data, and you could pick/choose which variables you wish to acquire.  The Hawthorne met data we get is 15-min data, while the air quality should be hourly (ozone may be 15min as well at time, actually if I remember right for Hawthorne).  To get more than a day you would need to log in/sign up for a MesoWest account.

2)  Or, you can get it from our new MesoWest API services (http://mesowest.org/api/).  The API provides a bit more flexibility in terms of getting data from multiple stations at once, choosing specific time periods, etc.  It is actually the tool I use to plot fixed site data on the sites.

At present, there is a CSV output option for singular station requests in the API (all other requests return in JSON format).  For example, here is Hawthorne (station ID QHW) data from 0 UTC 1 Feb to 0 UTC 10 Feb.

http://api.mesowest.net/v2/stations/timeseries?&token=demotoken&start=201602010000&end=201602100000&obtimezone=utc&output=csv&stid=QHW

If all you wanted was PM2.5, you could add "&vars=PM_25_concentration" to the end of that link to get this:

http://api.mesowest.net/v2/stations/timeseries?&token=demotoken&start=201602010000&end=201602100000&obtimezone=utc&output=csv&stid=QHW&vars=PM_25_concentration

The start and end times need to be in UTC, but you can choose "&obtimezone=local" instead of "&obtimezone=utc" to have the data returned in MDT/MST instead of UTC if desired.  Personally, I like to keep things simple when I'm doing queries and always return data in UTC, but that is just a personal preference.

Hope some of this info is helpful, and let John or I know if there are any specific questions as well.

-Alex

Alexander Jacques
Ph.D. Candidate - Atmospheric Sciences
Programming Support - MesoWest
University of Utah
INSCC Rm. 484
(801) 581-4362
Alexander.Jacques@utah.edu

On Tue, Feb 9, 2016 at 11:45 PM, John C. Lin <John.Lin@utah.edu> wrote:
Hi Alex,

Quick question:  just wondering where I could turn to for downloading the recent Hawthorne PM2.5 observations?
I was hoping to do some analyses myself of PM variations in recent months.

I've been following the concentrations on DAQ's website, but haven't found out a way to download the data.
Can you tell me where to download the data?
Or you have been grabbing the observations from the HTML file displayed on their real-time website? If so, could you point me to where on CHPC I could perhaps access the data files?

Thanks,

John

