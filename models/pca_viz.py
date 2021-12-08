from sklearn.preprocessing import normalize

def add_correlation_circle(figure, coeffs, texts, normalization=True, add_circle=True):
    if add_circle:
        figure.add_shape(type="circle",
                         xref="x", yref="y",
                         x0=-1, y0=-1, x1=1, y1=1,
                         line_color="blue"
                        )
    if normalization:
        coeffs = normalize(coeffs,axis=0)
    for i in range(coeffs.shape[1]):
        figure.add_annotation(
            x=coeffs[0,i],  # arrows' head
            y=coeffs[1,i],  # arrows' head
            ax=0,  # arrows' tail
            ay=0,  # arrows' tail
            xref='x',
            yref='y',
            axref='x',
            ayref='y',
            text='',  # if you want only the arrow
            showarrow=False,
            arrowhead=2,
            arrowsize=1,
            arrowwidth=2,
            arrowcolor='red'
        )

        figure.add_annotation(
            x=coeffs[0,i]*1.25, 
            y=coeffs[1,i]*1.25,
            text=texts[i],
            showarrow=False,
            font=dict(size=10,color="red")
        )
    return figure