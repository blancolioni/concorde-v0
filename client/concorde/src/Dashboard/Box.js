export default class Box {

    constructor(id, left, top = null, right = null, bottom = null) {
        this.id = id;
        if (typeof left === 'object') {
            this.anchor = {
                ...left,
            }
        } else {
            this.anchor = {
                left: left,
                top: top,
                right: right,
                bottom: bottom,
            }
        }
        this.childBoxes = null;
        this.childComponent = null;

        this.splitVertical = this.splitVertical.bind(this);
        this.concatLeaves = this.concatLeaves.bind(this);
        this.mapLeaves = this.mapLeaves.bind(this);
    }

    splitVertical(topId,bottomId) {
        const { left, top, right, bottom } = this.anchor;
        const newBox = new Box(this.id, this.anchor);
        const topChild = new Box(topId, left, top, right, top + (bottom - top) / 2);
        topChild.childComponent = this.childComponent;
        const bottomChild = new Box(bottomId, left, topChild.anchor.bottom, right, bottom);
        newBox.childBoxes = [topId, bottomId];
        return [ newBox, topChild, bottomChild];
    }

    concatLeaves(get) {
        if (this.childBoxes) {
            let acc = []
            for (const child of this.childBoxes) {
                acc = acc.concat(get(child).concatLeaves(get));
            }
            return acc;
        } else {
            return [this];
        }

    }

    mapLeaves(get, f) {
        let acc = this.concatLeaves(get, this);
        console.log(acc);
        return acc.map(f);
    }

    setChildComponent(title, view, model, client) {
        this.childComponent = { title, view, model, client };
    }

}
