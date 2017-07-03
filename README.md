
Call out in the docs that you need to install a dev version of ggmap.

I identified 76 stops in the gtfs data that are not contained in the
polygon data for census data for King County.

---

Thanks Carl for deepening your analysis a bit.  This is really a fun
package, and a fun application of some of the skills you've acquired
this year.   Congratulations on a fine project and a successful
completion of the Stat R certificate.   Yeah - it sounds anticlimactic.
How about congratulations on certifiable Stats and R programming
pseudo-gurudom?   May you use you newfound powers only for good. 

Let me make some quick comments on your analysis:  I would not use a 3rd
order (or any order) polynomial for these data.  If you look at your
density / density plot, you see that there's sort of a saturation point
of coverage (your horizontal green line).  That's pretty much downtown -
there are only so many lines you can send up every single street!  You
could fit a curve that saturates in that way, e.g.   

    alpha(1 - exp(-beta*x))

where alpha is that saturation, and beta is the characteristic density
at which you saturate.  That model is easy to fit with "nls" (or your
own likelihood function).  For longitude and latitude - even though I'm
not usually a big fan, I would use a low-information "gam" (or even
loess) because that would let you visualize the spatial structure of the
distribution of density.  Seattle is a bit "strange" because of the
water, but ultimately it is pretty symmetrically spread density wise
north-south, and the mean trend east-west and a decrease to the east.
You could (should!) capture those patterns somehow (again - I am
allergic to polynomials), so that you can then really ask:  for
similarly "outlying" areas, is bus service appropriately linked to
density.  A useful tool would take those outcomes and maybe highlight
hotspots of underserved locations.  Obviously, a real analysis would
need demographic and economic data as well. 

Anyways, its been fun having you in class.  I wish you best of luck with
your future endeavors. 

Best,
Elie

# --- END --- #

