(in-package #:kd)

(defun-with-dbg sah (patch aabb axis-index triangles)
  (let* ((aabb (corners aabb))
         (div-position  (+ 0.25 (random 0.5))))
    (+ (nth axis-index aabb)
       (* div-position (- (nth (+ 3 axis-index) aabb)
                          (nth axis-index aabb))))))

#|
bestpos = -1
bestcost = 1000000

for (each primitive in node)
{
left_extreme = primitive->getleftextreme( axis )
right_extreme = primitive->getrightextreme( axis )
if (cost = calculatecost( left_extreme ) < bestcost)
bestcost = cost, bestpos = left_extreme

if (cost = calculatecost( right_extreme ) < bestcost)
bestcost = cost, bestpos = right_extreme
}

float calculatecost( splitpos )
{
leftarea = calculateleftarea( splitpos )
rightarea = calculaterightarea( splitpos )
leftcount = calculateleftprimitivecount( splitpos )
rightcount = calculaterightprimitivecount( splitpos )
return costtrav + costintersect * (leftarea * leftcount + rightarea * rightcount)
}
|#


