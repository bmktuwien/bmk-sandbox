/**
 * Created by bmk on 10/25/15.
 */

var data = [
    {radius: 200, segment: 'Segment A'},
    {radius: 74, segment: 'Segment A'},
    {radius: 200, segment: 'Segment A'},
    {radius: 200, segment: 'Segment A'},
    {radius: 100, segment: 'Segment A'},
    {radius: 280, segment: 'Segment B'},
    {radius: 120, segment: 'Segment C'},
    {radius: 240, segment: 'Segment C'},
    {radius: 120, segment: 'Segment C'},
    {radius: 270, segment: 'Segment C'}
];




function render(data) {
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

    var color = d3.scale.category20c();

    var width = 1024,
        height = 768,
        radius = Math.min(width, height) / 2,
        originX = width / 2,
        originY = height / 2 + 10;

    var svg = d3.select("body").append("svg")
        .attr("width", width)
        .attr("height", height)
        .append("g")
        .attr("transform", "translate(" +  originX + "," +  originY + ")");

    var scaleX = d3.scale.linear().range([0, Math.PI]);

    var arc = d3.svg.arc()
        .startAngle(function(d) { return angle(scaleX(d.x)); })
        .endAngle(function(d) { return angle(scaleX(d.x + d.dx)); })
        .innerRadius(0)
        .outerRadius(radius);

    // render segments
    svg.selectAll("path")
        .data(segments)
        .enter().append("path")
        .style("fill", function(d) { return color(d.label); })
        .attr("d", arc);

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
        .attr("cx", function(d) { return d.pos.x;})
        .attr("cy", function(d) { return d.pos.y;})
        .attr("r", function(d) {return d.radius;})

}

render(data);
