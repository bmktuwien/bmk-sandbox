/**
 * Created by bmk on 10/25/15.
 */

var data = [
    {radius: 200, segment: 'Segment A', label: 'Label 1', color: 'red'},
    {radius: 74,  segment: 'Segment A', label: 'Label 2', color: 'red'},
    {radius: 200, segment: 'Segment A', label: 'Label 3', color: 'yellow'},
    {radius: 200, segment: 'Segment A', label: 'Label 4', color: 'red'},
    {radius: 100, segment: 'Segment A', label: 'Label 5', color: 'blue'},
    {radius: 280, segment: 'Segment B', label: 'Label 6', color: 'green'},
    {radius: 120, segment: 'Segment C', label: 'Label 7', color: 'yellow'},
    {radius: 240, segment: 'Segment C', label: 'Label 8', color: 'red'},
    {radius: 120, segment: 'Segment C', label: 'Label 9', color: 'black'},
    {radius: 270, segment: 'Segment D', label: 'Label 10', color: 'black'},
    {radius: 270, segment: 'Segment D', label: 'Label 10', color: 'black'}
];




function render(data) {
    var width = 1024,
        height = 768,
        radius = Math.min(width, height) / 2;

    ///////////////////////////////////////////////////////////////////////////////////////////////

    // transform data into segments
    var segments = _.map(_.groupBy(data, 'segment'), function (group, key) {
        var size = _.max(_.values(_.countBy(group, 'radius'))) + 1;

        return {'label': key, 'size': size, 'group': group};
    });

    var totalSize =  _.reduce( _.pluck(segments, 'size'), function(memo, num){
        return memo + num;
    }, 0);

    var step = 1 / totalSize;

    // add dimension to segments
    for (var i = 0, x = 0; i < segments.length; i++) {
        segments[i].x = x;
        segments[i].dx = segments[i].size * step;
        x = segments[i].x + segments[i].dx;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    function angle(a) {
        a = Math.max(0, Math.min(Math.PI, a));
        //rotate 90% counter clockwise
        return a - (Math.PI / 2);
    }

    var color = d3.scale.category10();

    var svg = d3.select("body").append("svg")
        .attr("width", width)
        .attr("height", height)
        .append("g")
        .attr("transform", "translate(" +  (width / 2) + "," +  (height / 2 + 50) + ")");

    var scaleX = d3.scale.linear().range([0, Math.PI]);

    var arc = d3.svg.arc()
        .startAngle(function(d) { return angle(scaleX(d.x)); })
        .endAngle(function(d) { return angle(scaleX(d.x + d.dx)); })
        .innerRadius(0)
        .outerRadius(radius);

    // render segments
    var arcs = svg.selectAll("path")
        .data(segments)
        .enter().append("g");

    arcs.append("path")
        .style("fill", function(d) { return color(d.label); })
        .attr("d", arc);

    arcs.append("svg:text")
        .attr("transform", function(d) {
            var c = arc.centroid(d),
                x = c[0],
                y = c[1],
                labelr = radius + 30,
                // pythagorean theorem for hypotenuse
                h = Math.sqrt(x*x + y*y);
            return "translate(" + (x/h * labelr) +  ',' + (y/h * labelr) +  ")";
        })
        .attr("dy", ".35em")
        .attr("text-anchor", function(d) {
            var endAngle = angle(scaleX(d.x + d.dx));
            return endAngle  < 0 ?
                "end" : "start";
        })
        .text(function(d) { return d.label; });

    ///////////////////////////////////////////////////////////////////////////////////////////////

    function calcPoint(a, r) {
        var x = 0 + r * Math.cos(a),
            y = 0 + r * Math.sin(a);

        return { 'x': x, 'y': y};
    }

    // calculate circles
    var circles = _.flatten(_.map(segments, function(segment) {
        return _.map(_.groupBy(segment.group,'radius'), function(trends, r) {
            return _.map(trends, function(trend, i) {
                var a = angle(scaleX(segment.x + step * (i+1))) - (Math.PI / 2);
                return {
                    'label': trend.label,
                    'color': trend.color,
                    'pos': calcPoint(a, r),
                    'radius': 7
                };
            });
        });
    }));

    // render circles
    svg.selectAll("circle")
        .data(circles)
        .enter().append("circle")
        .attr("cx", function(d) { return d.pos.x; })
        .attr("cy", function(d) { return d.pos.y; })
        .attr("r", function(d) { return d.radius; })
        .attr("fill", function(d){ return d.color; });

    ///////////////////////////////////////////////////////////////////////////////////////////////

    // render right-half x axis

    var years = d3.scale.ordinal()
        .domain([0,1,2,3,4])
        .rangePoints([2000, 2015])
        .range();

    years = _.map(years, Math.floor);

    var xAxisScale = d3.scale.ordinal()
        .domain(years)
        .rangePoints([0, radius]);

    var xAxis = d3.svg.axis()
        .scale(xAxisScale)
        .orient("bottom");

    svg.append("g")
        .attr("class", "x axis")
        .call(xAxis);

    // render left-half x axis

    var yearsRev = years.reverse();
    yearsRev[yearsRev.length - 1] = ''; // quick-fix not to render minYear twice

    var xAxisScaleRev = d3.scale.ordinal()
        .domain(yearsRev)
        .rangePoints([0, radius]);

    var xAxisRev = d3.svg.axis()
        .scale(xAxisScaleRev)
        .orient("bottom");

    svg.append("g")
        .attr("class", "x axis")
        .attr("transform", "translate(-" + radius + ",0)")
        .call(xAxisRev);

}

render(data);
