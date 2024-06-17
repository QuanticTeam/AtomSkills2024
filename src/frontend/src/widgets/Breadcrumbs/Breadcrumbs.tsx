import { Breadcrumb } from 'antd'
import { NavLink, UIMatch, useMatches } from 'react-router-dom'

export function Breadcrumbs() {
  const matches = useMatches() as UIMatch<unknown, Record<string, any>>[]

  const crumbs = matches
    .filter(match => Boolean(match.handle?.crumb))
    .map(match => [match.pathname, match.handle.crumb()])
    .map(([pathname, { title }]) => ({
      title: (
        <NavLink
          to={pathname}
          className={({ isActive }) => (!isActive ? '!text-link-1' : '')}
          end
        >
          {title}
        </NavLink>
      ),
    }))

  return (
    <Breadcrumb
      items={[
        {
          title: '',
        },
        ...crumbs,
      ]}
      className="p-6"
    />
  )
}
